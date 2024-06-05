package com.logicaldoc.util.io;

import java.nio.charset.Charset;

public class CharsetUtil {

	private CharsetUtil() {
		throw new IllegalStateException("Utility class");
	}

	static public Charset utf8() {
		return Charset.forName("UTF-8");
	}
}