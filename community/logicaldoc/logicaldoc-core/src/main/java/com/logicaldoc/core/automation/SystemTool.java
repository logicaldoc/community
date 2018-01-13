package com.logicaldoc.core.automation;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.util.exec.Exec;

/**
 * Utility functions for the system
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class SystemTool {

	public String execGetOutput(String commandline) throws IOException {
		return execGetOutput(commandline, null);
	}

	public String execGetOutput(String commandline, String path) throws IOException {
		return Exec.exec(commandline, null, path != null ? new File(path) : null);
	}

	public int exec(String commandline) throws IOException {
		int ret = Exec.exec(commandline, null, null, -1);
		return ret;
	}

	public int exec(String commandline, String path) throws IOException {
		int ret = Exec.exec(commandline, null, new File(path), -1);
		return ret;
	}
}