package com.logicaldoc.core.automation;

import java.io.File;
import java.io.IOException;
import java.util.List;

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
	 * Executes a command and gets it's output. The path of the command must be
	 * listed in the allowed-commands.txt
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
	 * @param commandline the command to execute. The path of the command must
	 *        be listed in the allowed-commands.txt
	 * @param path the path to set as current folder before executing the
	 *        command
	 * 
	 * @return the output of the command
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public String execGetOutput(String commandline, String path) throws IOException {
		return new Exec().execGetOutput(commandline, null, path != null ? new File(path) : null);
	}

	/**
	 * Executes a command
	 * 
	 * @param commandline the command to execute. The path of the command must
	 *        be listed in the allowed-commands.txt
	 * 
	 * @return the execution's result
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public int exec(String commandline) throws IOException {
		return new Exec().exec(commandline, null, null, -1);
	}

	/**
	 * Executes a command
	 * 
	 * @param commandline the command to execute. The path of the command must
	 *        be listed in the allowed-commands.txt
	 * @param path the path to set as current folder before executing the
	 *        command
	 * 
	 * @return the execution's result
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public int exec(String commandline, String path) throws IOException {
		return new Exec().exec(commandline, null, new File(path), -1);
	}

	/**
	 * Executes the command
	 * 
	 * @param commandLine the list of elements in the command line. The path of
	 *        the command must be listed in the allowed-commands.txt
	 * @param env the environment variables
	 * @param path the path to set as current folder before executing the
	 *        command
	 * @param timeout maximum execution time expressed in seconds
	 *
	 * @return the return code of the command
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public int exec(List<String> commandLine, List<String> env, String path, int timeout) throws IOException {
		return new Exec().exec(commandLine, env, new File(path), timeout);
	}

	/**
	 * Executes the command and gets it's output
	 * 
	 * @param commandLine the list of elements in the command line. The path of
	 *        the command must be listed in the allowed-commands.txt
	 * @param env the environment variables
	 * @param path the path to set as current folder before executing the
	 *        command
	 * @param timeout maximum execution time expressed in seconds
	 *
	 * @return the output of the command
	 * 
	 * @throws IOException error executing the command or command not found
	 */
	public String execGetOutput(List<String> commandLine, List<String> env, String path, int timeout)
			throws IOException {
		return new Exec().execGetOutput(commandLine, env, new File(path), timeout);
	}

}