package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Representation of a task
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUITask implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final int STATUS_IDLE = 0;

	public static final int STATUS_RUNNING = 1;

	public static final int STATUS_STOPPING = 2;

	private int status = STATUS_IDLE;

	private long size = 0;

	private int progress = 0;

	private GUIScheduling scheduling;

	private String name;

	private String schedulingLabel;

	private boolean indeterminate = false;

	private int completionPercentage = 0;

	private boolean sendActivityReport = false;

	private List<GUIUser> reportRecipients = new ArrayList<>();

	public List<GUIUser> getReportRecipients() {
		return reportRecipients;
	}

	public void setReportRecipients(List<GUIUser> reportRecipients) {
		this.reportRecipients = reportRecipients;
	}

	public void addReportRecipient(GUIUser user) {
		if (reportRecipients.stream().noneMatch(u -> u.getName().equals(user.getName())))
			reportRecipients.add(user);
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