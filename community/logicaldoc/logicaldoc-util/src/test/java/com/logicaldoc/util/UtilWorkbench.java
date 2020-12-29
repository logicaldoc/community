package com.logicaldoc.util;

import java.io.IOException;

import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.security.PasswordGenerator;

public class UtilWorkbench {

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		String command="C:\\LogicalDOC-Devel\\imagemagick\\convert.exe -compress JPEG -quality 90 -resize x200 C:\\tmp\\test.jpg C:\\tmp\\tile.jpg";
		Exec.exec(command, null, null, 10);
	}
}