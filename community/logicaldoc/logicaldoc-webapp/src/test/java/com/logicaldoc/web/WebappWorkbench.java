package com.logicaldoc.web;

import java.io.IOException;

public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
		int cores = Runtime.getRuntime().availableProcessors();
		System.out.println("cores: " + cores);

	}
}
