package com.logicaldoc.util.exec;

/**
 * A simple thread that waits for completion of a given Process.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
public class Worker extends Thread {
	private final Process process;

	private Integer exit;

	public Worker(Process process) {
		this.process = process;
	}

	public void run() {
		try {
			exit = process.waitFor();
		} catch (InterruptedException ignore) {
			return;
		}
	}

	public Integer getExit() {
		return exit;
	}
}
