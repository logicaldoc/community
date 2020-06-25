package com.logicaldoc.util;

import java.io.IOException;

import com.logicaldoc.util.security.PasswordGenerator;

public class UtilWorkbench {

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		System.out.println(PasswordGenerator.generate(16));
	}
}