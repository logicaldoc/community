package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.util.Base64;

import org.apache.commons.io.FileUtils;


public class WebappWorkbench {
	public static void main(String[] args) throws IOException {

		
		String installDir="C:\\LogicalDOC";
		installDir = installDir.replace("\\", "/");
		System.out.println(installDir);
	}
}
