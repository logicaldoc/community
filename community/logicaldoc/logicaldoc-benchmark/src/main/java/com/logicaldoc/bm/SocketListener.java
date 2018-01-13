package com.logicaldoc.bm;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;

public class SocketListener extends Thread {

	private MultiLoader loader;

	private int port;

	public SocketListener(MultiLoader loader, int port) {
		this.loader = loader;
		this.port = port;
	}

	@Override
	public void run() {

		// declaration section:
		// declare a server socket and a client socket for the server
		// declare an input and an output stream
		ServerSocket server = null;
		String command;
		DataInputStream is;
		PrintStream os;
		Socket clientSocket = null;

		// Try to open a server socket on port 9999
		// Note that we can't choose a port less than 1023 if we are not
		// privileged users (root)
		try {
			server = new ServerSocket(port);
		} catch (IOException e) {
			System.out.println(e);
		}

		// Create a socket object from the ServerSocket to listen and accept
		// connections.
		// Open input and output streams
		try {
			clientSocket = server.accept();
			is = new DataInputStream(clientSocket.getInputStream());
			os = new PrintStream(clientSocket.getOutputStream());

			// As long as we receive data, echo that data back to the client.
			while (true) {
				command = is.readLine();

				if (command == null || command.isEmpty()) {
					clientSocket = server.accept();
					is = new DataInputStream(clientSocket.getInputStream());
					os = new PrintStream(clientSocket.getOutputStream());
				} else {
					System.out.println("Received command from socket: " + command);
					try {
						os.println(loader.processCommand(command));
						os.println("\n");
					} catch (InterruptedException e) {
					}
				}
			}
		} catch (IOException e) {
			System.out.println(e);
		}
	}
}