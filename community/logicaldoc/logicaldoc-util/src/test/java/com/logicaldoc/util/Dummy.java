package com.logicaldoc.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;

public class Dummy {

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		// Map<String, Charset> charsets = Charset.availableCharsets();
		// for (String name : charsets.keySet()) {
		// System.out.println(name + " " + charsets.get(name).displayName());
		// }

		File tmp = File.createTempFile("out", "");
		FileWriter sw = null;
		try {
			sw = new FileWriter(tmp);
			Exec.exec("D:\\tmp\\dir.bat", null, null, sw, 50);
			sw.flush();
		} finally {
			System.out.println(FileUtil.readFile(tmp));
			FileUtil.strongDelete(tmp);
		}
	}
}