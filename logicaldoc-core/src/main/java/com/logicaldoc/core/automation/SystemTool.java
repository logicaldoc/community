package com.logicaldoc.core.automation;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.util.exec.Exec;

/**
 * Utility functions for interacting with the Operative System from within the
 * Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
@AutomationDictionary
public class SystemTool {

	/**
	 * Executes a command and gets it's output. The path of the command must be listed in the allowed-commands.txt
	 * 
	 * @param commandline the command to execute
	 * 
	 * @return the output of the command
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public String execGetOutput(String commandline) throws IOException {
		return execGetOutput(commandline, null);
	}

	/**
	 * Executes a command and gets it's output
	 * 
	 * @param commandline the command to execute. The path of the command must be listed in the allowed-commands.txt
	 * @param path the path to set as current folder before executing the
	 *        command
	 * 
	 * @return the output of the command
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public String execGetOutput(String commandline, String path) throws IOException {
		return new Exec().exec(commandline, null, path != null ? new File(path) : null);
	}

	/**
	 * Executes a command
	 * 
	 * @param commandline the command to execute. The path of the command must be listed in the allowed-commands.txt
	 * 
	 * @return the execution's result
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public int exec(String commandline) throws IOException {
		int ret = new Exec().exec(commandline, null, null, -1);
		return ret;
	}

	/**
	 * Executes a command
	 * 
	 * @param commandline the command to execute. The path of the command must be listed in the allowed-commands.txt
	 * @param path the path to set as current folder before executing the
	 *        command
	 * 
	 * @return the execution's result
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public int exec(String commandline, String path) throws IOException {
		int ret = new Exec().exec(commandline, null, new File(path), -1);
		return ret;
	}
}