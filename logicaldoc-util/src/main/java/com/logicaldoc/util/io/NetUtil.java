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

	/**
	 * Checks if a port is available
	 * 
	 * @param port The port to check
	 * 
	 * @return true id the port is available
	 */
	public static boolean available(int port) {
		ServerSocket socket = null;
		Socket client = null;
		try {
			try {
				client = new Socket("127.0.0.1", port);
				return false;
			} catch (Exception e) {
				socket = new ServerSocket(port);
				socket.close();
				return true;
			}
		} catch (Exception e) {
			log.error(e.getMessage());
			return false;
		} finally {
			// Clean up
			if (socket != null)
				try {
					socket.close();
				} catch (IOException e) {
					// Nothing to do
				}
			if (client != null)
				try {
					client.close();
				} catch (IOException e) {
					// Nothing to do
				}
		}
	}
}
