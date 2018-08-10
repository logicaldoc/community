package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a task
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUITask implements Serializable {

	private static final long serialVersionUID = 1L;

	public final static int STATUS_IDLE = 0;

	public final static int STATUS_RUNNING = 1;

	public final static int STATUS_STOPPING = 2;

	private int status = STATUS_IDLE;

	private long size = 0;

	private int progress = 0;

	private GUIScheduling scheduling;

	private String name;

	private String schedulingLabel;

	private boolean indeterminate = false;

	private int completionPercentage = 0;

	private boolean sendActivityReport = false;

	private GUIUser[] reportRecipients = new GUIUser[0];

	public GUIUser[] getReportRecipients() {
		return reportRecipients;
	}

	public void setReportRecipients(GUIUser[] reportRecipients) {
		this.reportRecipients = reportRecipients;
	}

	public void addReportRecipient(GUIUser user) {
		GUIUser[] tmp = new GUIUser[reportRecipients.length + 1];
		int i = 0;
		for (GUIUser u : reportRecipients) {
			// Skip if the user already exists
			if (u.getUserName().equals(user.getUserName()))
				return;
			tmp[i++] = u;
		}
		tmp[i] = user;
		reportRecipients = tmp;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public int getProgress() {
		return progress;
	}

	public void setProgress(int progress) {
		this.progress = progress;
	}

	public GUIScheduling getScheduling() {
		return scheduling;
	}

	public void setScheduling(GUIScheduling scheduling) {
		this.scheduling = scheduling;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getSchedulingLabel() {
		return schedulingLabel;
	}

	public void setSchedulingLabel(String schedulingLabel) {
		this.schedulingLabel = schedulingLabel;
	}

	public boolean isIndeterminate() {
		return indeterminate;
	}

	public void setIndeterminate(boolean indeterminate) {
		this.indeterminate = indeterminate;
	}

	public int getCompletionPercentage() {
		return completionPercentage;
	}

	public void setCompletionPercentage(int completionPercentage) {
		this.completionPercentage = completionPercentage;
	}

	public boolean isSendActivityReport() {
		return sendActivityReport;
	}

	public void setSendActivityReport(boolean sendActivityReport) {
		this.sendActivityReport = sendActivityReport;
	}

}