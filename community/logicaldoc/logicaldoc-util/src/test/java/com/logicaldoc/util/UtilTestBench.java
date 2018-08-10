package com.logicaldoc.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;

public class UtilTestBench {

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		File a = new File("C:\\tmp\\test.txt");
		File b = new File("C:\\tmp\\out.txt");
		FileUtil.copy(a, b, 4);
	}
}