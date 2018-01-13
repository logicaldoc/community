package com.logicaldoc.util;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

public class HtmlColor {
	private static Map<String, Color> colorNames;

	static {
		// color names.
		colorNames = new HashMap<String, Color>();
		colorNames.put("black", new Color(0x000000));
		colorNames.put("green", new Color(0x008000));
		colorNames.put("silver", new Color(0xC0C0C0));
		colorNames.put("lime", new Color(0x00FF00));
		colorNames.put("gray", new Color(0x808080));
		colorNames.put("olive", new Color(0x808000));
		colorNames.put("white", new Color(0xFFFFFF));
		colorNames.put("yellow", new Color(0xFFFF00));
		colorNames.put("maroon", new Color(0x800000));
		colorNames.put("navy", new Color(0x000080));
		colorNames.put("red", new Color(0xFF0000));
		colorNames.put("blue", new Color(0x0000FF));
		colorNames.put("purple", new Color(0x800080));
		colorNames.put("teal", new Color(0x008080));
		colorNames.put("fuchsia", new Color(0xFF00FF));
		colorNames.put("aqua", new Color(0x00FFFF));
	}

	/**
	 * Decode HTML-attribute style of color to {@link Color}
	 * 
	 * @param color - color name or #RRGGBB string
	 * @return - color for this value.
	 */
	public static Color decode(String color) {
		if (null == color) {
			throw new IllegalArgumentException("NULL_COLOR_PARAMETER_ERROR");
		}
		Color c = (Color) colorNames.get(color.trim().toLowerCase());
		if (null == c) {
			try {
				c = Color.decode(color.trim());
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("DECODE_COLOR_PARAMETER_ERROR");
			}
		}
		return c;
	}

	public static Integer integerValue(String color) {
		return new Integer(decode(color).getRGB());
	}

	public static String encodeRGB(Color color) {
		if (null == color) {
			throw new IllegalArgumentException("NULL_COLOR_PARAMETER_ERROR_2");
		}
		return "#" + Integer.toHexString(color.getRGB()).substring(2).toUpperCase();
	}

}