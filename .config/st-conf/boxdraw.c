/*
 * Copyright 2018 Avi Halachmi (:avih) avihpit@yahoo.com https://github.com/avih
 * MIT/X Consortium License
 */

#include <X11/Xft/Xft.h>
#include <X11/Xlib.h>
#include <stdint.h>

#include "st.h"
#include "boxdraw_data.h"

/* config.h */
extern int boxdraw, boxdraw_bold, boxdraw_braille;

static Display *xdpy;
static Colormap xcmap;
static XftDraw *xdraw;
static Visual *xvis;

static void drawbox(int, int, int, int, XftColor *, XftColor *, ushort);
static void drawboxlines(int, int, int, int, XftColor *, ushort);

/* public API */

void
boxdraw_xinit(Display *dpy, Colormap cmap, XftDraw *draw, Visual *vis)
{
	xdpy = dpy; xcmap = cmap; xdraw = draw; xvis = vis;
}

int
isboxdraw(Rune u)
{
	Rune block = u & ~0xff;
	return (boxdraw && block == 0x2500 && boxdata[(uint8_t)u]) ||
	       (boxdraw_braille && block == 0x2800);
}

ushort
boxdrawindex(const Glyph *g)
{
	if (boxdraw_braille && (g->u & ~0xff) == 0x2800)
		return BRL | (uint8_t)g->u;

	return (g->mode & ATTR_BOLD && boxdraw_bold)
	       ? (BDB | boxdata[(uint8_t)g->u])
	       : boxdata[(uint8_t)g->u];
}

void
drawboxes(int x, int y, int cw, int ch, XftColor *fg, XftColor *bg,
          const XftGlyphFontSpec *specs, int len)
{
	for (int i = 0; i < len; i++) {
		drawbox((int)specs[i].x, (int)specs[i].y - ch + 1, cw, ch,
		        fg, bg, (ushort)(uintptr_t)specs[i].font);
	}
}

/* private */

static void
drawbox(int x, int y, int w, int h, XftColor *fg, XftColor *bg, ushort bd)
{
	ushort cat = bd & ~0xff;
	ushort data = bd & 0xff;

	/* fill background */
	XftDrawRect(xdraw, bg, x, y, w, h);

	if (cat == BRL) {
		/* Braille: 8 dots in 2 columns x 4 rows */
		int dotw = MAX(1, w / 4);
		int doth = MAX(1, h / 8);
		int xoff[2] = { w / 4 - dotw / 2, w * 3 / 4 - dotw / 2 };
		int yoff[8] = {
			h * 1 / 8 - doth / 2, h * 2 / 8 - doth / 2,
			h * 3 / 8 - doth / 2, h * 4 / 8 - doth / 2,
			h * 5 / 8 - doth / 2, h * 6 / 8 - doth / 2,
			h * 7 / 8 - doth / 2, h * 8 / 8 - doth / 2,
		};
		/* U+2800 bits 0-3: left col rows 1-4; bits 4-6: right col rows 1-3; bit 7: right row 4 */
		for (int i = 0; i < 4; i++)
			if (data & (1 << i))
				XftDrawRect(xdraw, fg, x + xoff[0], y + yoff[i], dotw, doth);
		for (int i = 0; i < 3; i++)
			if (data & (1 << (i + 4)))
				XftDrawRect(xdraw, fg, x + xoff[1], y + yoff[i], dotw, doth);
		if (data & (1 << 7))
			XftDrawRect(xdraw, fg, x + xoff[1], y + yoff[3], dotw, doth);
		return;
	}

	if (cat == BBS) {
		/* Shades: blend fg into bg */
		XRenderColor rc;
		XftColor shxft;
		int a = (data == 1) ? 64 : (data == 2) ? 128 : 192;
		rc.red   = (fg->color.red   * a + bg->color.red   * (255 - a)) / 255;
		rc.green = (fg->color.green * a + bg->color.green * (255 - a)) / 255;
		rc.blue  = (fg->color.blue  * a + bg->color.blue  * (255 - a)) / 255;
		rc.alpha = 0xffff;
		XftColorAllocValue(xdpy, xvis, xcmap, &rc, &shxft);
		XftDrawRect(xdraw, &shxft, x, y, w, h);
		XftColorFree(xdpy, xvis, xcmap, &shxft);
		return;
	}

	/* Block fractions */
	if (cat == BBD || cat == BBU || cat == BBL || cat == BBR) {
		int n = data & 0xf;
		if (n) {
			int fx = x, fy = y, fw = w, fh = h;
			if      (cat == BBD) { fh = h * n / 8; fy = y + h - fh; }
			else if (cat == BBU) { fh = h * n / 8; }
			else if (cat == BBL) { fw = w * n / 8; }
			else                 { fw = w * n / 8; fx = x + w - fw; }
			XftDrawRect(xdraw, fg, fx, fy, fw, fh);
		}
		return;
	}

	if (cat == BBQ) {
		/* Quadrants: TL=1 TR=2 BL=4 BR=8 */
		int hw = w / 2, hh = h / 2;
		if (data & TL) XftDrawRect(xdraw, fg, x,      y,      w - hw, h - hh);
		if (data & TR) XftDrawRect(xdraw, fg, x + hw, y,      hw,     h - hh);
		if (data & BL) XftDrawRect(xdraw, fg, x,      y + hh, w - hw, hh);
		if (data & BR) XftDrawRect(xdraw, fg, x + hw, y + hh, hw,     hh);
		return;
	}

	if (cat & (BDL | BDA)) {
		drawboxlines(x, y, w, h, fg, bd);
		return;
	}
}

static void
drawboxlines(int x, int y, int w, int h, XftColor *fg, ushort bd)
{
	/* thickness: light ~13% of cell, double-gap ~13%, heavy ~26% */
	int lw = MAX(1, (int)(0.13 * w + 0.5));
	int lh = MAX(1, (int)(0.13 * h + 0.5));

	int cx = x + w / 2;
	int cy = y + h / 2;

	if (bd & BDA) {
		/* Arcs are drawn as straight line stubs to center */
		ushort a = bd & 0xf;
		if (a & LL) XftDrawRect(xdraw, fg, x,         cy - lh/2, cx - x,    lh);
		if (a & LU) XftDrawRect(xdraw, fg, cx - lw/2, y,         lw, cy - y);
		if (a & LR) XftDrawRect(xdraw, fg, cx,        cy - lh/2, x + w - cx, lh);
		if (a & LD) XftDrawRect(xdraw, fg, cx - lw/2, cy,        lw, y + h - cy);
		return;
	}

	/* BDL: data = bitmask of line segments */
	ushort d = bd & 0xff;

	/* For each of 4 directions: determine line weight and draw */
	/* Left segment */
	if (d & LL) {
		int thick = (d & DL) ? lh * 2 : lh;  /* heavy if both L+D bits */
		int lx = x, rx = cx;
		XftDrawRect(xdraw, fg, lx, cy - thick/2, rx - lx, thick);
	} else if (d & DL) {
		/* double lines left */
		int gap = lh;
		XftDrawRect(xdraw, fg, x, cy - lh/2 - gap, cx - x, lh);
		XftDrawRect(xdraw, fg, x, cy - lh/2 + gap, cx - x, lh);
	}

	/* Right segment */
	if (d & LR) {
		int thick = (d & DR) ? lh * 2 : lh;
		XftDrawRect(xdraw, fg, cx, cy - thick/2, x + w - cx, thick);
	} else if (d & DR) {
		int gap = lh;
		XftDrawRect(xdraw, fg, cx, cy - lh/2 - gap, x + w - cx, lh);
		XftDrawRect(xdraw, fg, cx, cy - lh/2 + gap, x + w - cx, lh);
	}

	/* Up segment */
	if (d & LU) {
		int thick = (d & DU) ? lw * 2 : lw;
		XftDrawRect(xdraw, fg, cx - thick/2, y, thick, cy - y);
	} else if (d & DU) {
		int gap = lw;
		XftDrawRect(xdraw, fg, cx - lw/2 - gap, y, lw, cy - y);
		XftDrawRect(xdraw, fg, cx - lw/2 + gap, y, lw, cy - y);
	}

	/* Down segment */
	if (d & LD) {
		int thick = (d & DD) ? lw * 2 : lw;
		XftDrawRect(xdraw, fg, cx - thick/2, cy, thick, y + h - cy);
	} else if (d & DD) {
		int gap = lw;
		XftDrawRect(xdraw, fg, cx - lw/2 - gap, cy, lw, y + h - cy);
		XftDrawRect(xdraw, fg, cx - lw/2 + gap, cy, lw, y + h - cy);
	}
}
