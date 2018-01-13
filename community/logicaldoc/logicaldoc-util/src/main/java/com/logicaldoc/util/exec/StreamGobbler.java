package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * When Runtime.exec() won't.
 * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html
 */
public class StreamGobbler extends Thread {

	private InputStream is;

	protected static Logger log = LoggerFactory.getLogger(StreamGobbler.class);

	public StreamGobbler(InputStream is, Logger log) {
		this.is = is;
	}

	@Override
	public void run() {
		try {
			InputStreamReader isr = new InputStreamReader(is);
			BufferedReader br = new BufferedReader(isr);
			String line = null;
			while ((line = br.readLine()) != null) {
				System.out.println(line);
				log.debug(line);
			}
		} catch (IOException ioe) {
			log.error(ioe.getMessage());
		}
	}
}
