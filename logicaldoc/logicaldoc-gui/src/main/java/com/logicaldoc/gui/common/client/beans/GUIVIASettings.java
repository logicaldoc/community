package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Settings of the VIA engine
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.8
 */
public class GUIVIASettings implements Serializable {

	private static final long serialVersionUID = 1L;

	private boolean enabled=true;
	
	private int maxAttachments=5;
	
	private long maxAttachmentSize=1048576L;
	
	private GUIEmailAccount emailAccount=null;

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public int getMaxAttachments() {
		return maxAttachments;
	}

	public void setMaxAttachments(int maxAttachments) {
		this.maxAttachments = maxAttachments;
	}

	public long getMaxAttachmentSize() {
		return maxAttachmentSize;
	}

	public void setMaxAttachmentSize(long maxAttachmentSize) {
		this.maxAttachmentSize = maxAttachmentSize;
	}

	public GUIEmailAccount getEmailAccount() {
		return emailAccount;
	}

	public void setEmailAccount(GUIEmailAccount emailAccount) {
		this.emailAccount = emailAccount;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
}