package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Genaeric message to the user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIMessage implements Serializable {
			
	public static final int PRIO_LOW = 0;
	
	public static final int PRIO_WARN = 2;

	public static final int PRIO_INFO = 1;

	private static final long serialVersionUID = 1L;

	private long id;

	private String message;

	private String url = null;

	private int priority = PRIO_WARN;

	private String recipient;

	private String subject;

	private boolean confirmation = false;

	// If the message must be shown in the login screen
	private boolean showInLogin = true;

	// If the message must be shown in the user interface
	private boolean showInGUI = false;
	
	public boolean isShowInGUI() {
		return showInGUI;
	}

	public void setShowInGUI(boolean showInGUI) {
		this.showInGUI = showInGUI;
	}

	public boolean isShowInLogin() {
		return showInLogin;
	}

	public void setShowInLogin(boolean showInLogin) {
		this.showInLogin = showInLogin;
	}

	private Integer validity;

	public GUIMessage() {
	}

	public GUIMessage(String message, int priority) {
		this.message = message;
		this.priority = priority;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public int getPriority() {
		return priority;
	}

	public void setPriority(int priority) {
		this.priority = priority;
	}

	public String getRecipient() {
		return recipient;
	}

	public void setRecipient(String recipient) {
		this.recipient = recipient;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public boolean isConfirmation() {
		return confirmation;
	}

	public void setConfirmation(boolean confirmation) {
		this.confirmation = confirmation;
	}

	public Integer getValidity() {
		return validity;
	}

	public void setValidity(Integer validity) {
		this.validity = validity;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}
}