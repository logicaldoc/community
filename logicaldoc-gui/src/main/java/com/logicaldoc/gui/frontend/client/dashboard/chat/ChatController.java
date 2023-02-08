package com.logicaldoc.gui.frontend.client.dashboard.chat;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * Implements the Observer pattern to distribute events on the chat
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class ChatController {

	private static ChatController instance = new ChatController();

	private Set<ChatObserver> observers = new HashSet<>();

	private ChatController() {
	}

	public static ChatController get() {
		return instance;
	}

	public synchronized void addObserver(ChatObserver observer) {
		if (observer != null && !observers.contains(observer))
			observers.add(observer);
	}

	public synchronized void removeObserver(ChatObserver observer) {
		if (observer != null && observers.contains(observer))
			observers.remove(observer);
	}

	public void newMessage(long id, Date date, String username, String message) {
		for (ChatObserver observer : observers)
			try {
				observer.onMessage(id, date, username, message);
			} catch (Throwable t) {
				// Nothing to do
			}
	}
}