package com.logicaldoc.gui.common.client.controllers;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * Implements the Observer pattern to distribute events on the users
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class UserController {

	private static UserController instance = new UserController();

	private Set<UserObserver> observers = new HashSet<>();

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
			} catch (Exception t) {
				// Nothing to do
			}
	}

	public void loggedIn(String username) {
		for (UserObserver observer : observers)
			observer.onUserLogin(username);
	}

	public void loggedOut(String username) {
		for (UserObserver observer : observers)
			observer.onUserLogout(username);
	}
}