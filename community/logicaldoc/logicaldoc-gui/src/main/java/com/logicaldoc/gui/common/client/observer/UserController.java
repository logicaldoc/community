package com.logicaldoc.gui.common.client.observer;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * Implements the Observer pattern to distribute events on the users
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class UserController {

	private static UserController instance = new UserController();

	private List<UserObserver> observers = new ArrayList<UserObserver>();

	private UserController() {
	}

	public static UserController get() {
		return instance;
	}

	public synchronized void addObserver(UserObserver observer) {
		if (observer != null && !observers.contains(observer))
			observers.add(observer);
	}

	public synchronized void removeObserver(UserObserver observer) {
		if (observer != null && observers.contains(observer))
			observers.remove(observer);
	}

	public void changed(GUIUser user) {
		for (UserObserver observer : observers)
			try {
				observer.onUserChanged(user);
			} catch (Throwable t) {
			}
	}
}