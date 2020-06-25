package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * GUI representation of an e-mail to be sent
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIEmail implements Serializable {

	private static final long serialVersionUID = 1L;

	private String subject;

	private boolean sendAsTicket = false;

	private boolean zipCompression = false;

	private boolean pdfConversion = false;

	private String message;

	private Date sent;

	private Date received;

	private GUIUser user;

	private long[] docIds;

	private GUIContact from;

	private GUIContact replyTo;

	private GUIContact[] tos;

	private GUIContact[] ccs;

	private GUIContact[] bccs;

	private GUIDocument[] attachments;

	private boolean signed = false;

	public GUIEmail() {
	}

	public boolean isSigned() {
		return signed;
	}

	public void setSigned(boolean signed) {
		this.signed = signed;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public boolean isSendAsTicket() {
		return sendAsTicket;
	}

	public void setSendAsTicket(boolean sendAsTicket) {
		this.sendAsTicket = sendAsTicket;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public GUIUser getUser() {
		return user;
	}

	public void setUser(GUIUser user) {
		this.user = user;
	}

	public long[] getDocIds() {
		return docIds;
	}

	public void setDocIds(long[] docIds) {
		this.docIds = docIds;
	}

	public boolean isZipCompression() {
		return zipCompression;
	}

	public void setZipCompression(boolean zipCompression) {
		this.zipCompression = zipCompression;
	}

	public boolean isPdfConversion() {
		return pdfConversion;
	}

	public void setPdfConversion(boolean pdfConversion) {
		this.pdfConversion = pdfConversion;
	}

	public GUIContact getReplyTo() {
		return replyTo;
	}

	public void setReplyTo(GUIContact replyTo) {
		this.replyTo = replyTo;
	}

	public GUIContact[] getTos() {
		return tos;
	}

	public void setTos(GUIContact[] tos) {
		this.tos = tos;
	}

	public GUIContact[] getCcs() {
		return ccs;
	}

	public void setCcs(GUIContact[] ccs) {
		this.ccs = ccs;
	}

	public GUIContact[] getBccs() {
		return bccs;
	}

	public void setBccs(GUIContact[] bccs) {
		this.bccs = bccs;
	}

	public Date getSent() {
		return sent;
	}

	public void setSent(Date sent) {
		this.sent = sent;
	}

	public Date getReceived() {
		return received;
	}

	public void setReceived(Date received) {
		this.received = received;
	}

	public GUIContact getFrom() {
		return from;
	}

	public void setFrom(GUIContact from) {
		this.from = from;
	}

	public GUIDocument[] getAttachments() {
		return attachments;
	}

	public void setAttachments(GUIDocument[] attachments) {
		this.attachments = attachments;
	}
}