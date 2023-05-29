package com.logicaldoc.util.io;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Some network utility methods.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class NetUtil {
	protected static Logger log = LoggerFactory.getLogger(JarUtil.class);

	private NetUtil() {
	}

	/**
	 * Checks if a port is available
	 * 
	 * @param port The port to check
	 * 
	 * @return true id the port is available
	 */
	public static boolean available(int port) {
		try (Socket client = new Socket("127.0.0.1", port)) {
			return false;
		} catch (Exception e) {
			return createSocket(port);
		}
	}

	private static boolean createSocket(int port) {
		try (ServerSocket socket = new ServerSocket(port)) {
			socket.close();
			return true;
		} catch (IOException e) {
			return false;
		}
	}
}