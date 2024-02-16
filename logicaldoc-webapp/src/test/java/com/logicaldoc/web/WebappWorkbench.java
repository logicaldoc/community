package com.logicaldoc.web;

import java.io.IOException;
import java.util.regex.Pattern;

public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
		Pattern scriptPattern = Pattern.compile("[^a-z^0-9^\\-]", Pattern.CASE_INSENSITIVE);

		String value = "Alj-hajr(d'ddddddd";
		value = scriptPattern.matcher(value).replaceAll("");
		System.out.println(value);
	}
}
