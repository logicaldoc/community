package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class WinUtil {
	private static final String TASKLIST = "tasklist";

	private static final String KILL = "taskkill /F /T /IM ";

	public static void main(String[] args) {
		if ("kill".equals(args[0])) {
			if (isProcessRunning(args[1])) {
				System.exit(killProcess(args[1]));
			}
		}
		System.exit(0);
	}

	public static boolean isProcessRunning(String imageName) {
		try {
			Process p = Runtime.getRuntime().exec(TASKLIST);

			BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line;
			while ((line = reader.readLine()) != null) {
				if (line.toLowerCase().contains(imageName.toLowerCase())) {
					return true;
				}
			}
			return false;
		} catch (IOException e) {
			return false;
		}
	}

	public static int killProcess(String imageName) {
		try {
			Process p = Runtime.getRuntime().exec(KILL + imageName);
			return p.exitValue();
		} catch (Throwable e) {
			return 1;
		}
	}
}
