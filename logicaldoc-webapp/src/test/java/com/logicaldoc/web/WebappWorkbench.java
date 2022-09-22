package com.logicaldoc.web;

import java.io.IOException;
import java.util.regex.Pattern;


public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
		
		String value="px2e%22-alert(1).\"-%22Hhnmlz'";
		Pattern scriptPattern = Pattern.compile("[^a-z^A-Z^0-9^-]", Pattern.CASE_INSENSITIVE);
		value = scriptPattern.matcher(value).replaceAll("");
System.out.println(value);
	}
}
