package com.logicaldoc.webservice.doc.util;

import java.util.Collection;

public class MyClassUtils {

	private MyClassUtils() {
	}

	public static boolean isClassArrayOrCollection(Class<?> clazz) {
		return clazz.isArray() || Collection.class.isAssignableFrom(clazz);
	}
}