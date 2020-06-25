package com.logicaldoc.web;

import java.awt.GraphicsEnvironment;
import java.io.IOException;

public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
		String fonts[] = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();

		for (int i = 0; i < fonts.length; i++) {
			System.out.println(fonts[i]);
		}

	}
}
