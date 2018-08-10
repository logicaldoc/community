package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Implementation of a retention policy for the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2
 */
public class GUIRetentionPolicy implements Serializable {
	private static final long serialVersionUID = 1L;

	public final static int DATE_OPT_CREATION = 0;

	public final static int DATE_OPT_PUBLISHED = 1;

	public final static int DATE_OPT_STOPPUBLISHING = 2;

	public final static int DATE_OPT_ARCHIVED = 3;

	public final static int ACTION_ARCHIVE = 0;

	public final static int ACTION_UNPUBLISH = 1;

	public final static int ACTION_DELETE = 2;

	private String name;

	private long id;

	private int position = 0;

	private Long templateId;

	private String templateName;

	private Long folderId;

	private String folderName;

	private int dateOption = DATE_OPT_CREATION;

	private int retentionDays = 1825;

	private int action = ACTION_DELETE;

	private int enabled = 1;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public int getDateOption() {
		return dateOption;
	}

	public void setDateOption(int dateOption) {
		this.dateOption = dateOption;
	}

	public int getRetentionDays() {
		return retentionDays;
	}

	public void setRetentionDays(int retentionDays) {
		this.retentionDays = retentionDays;
	}

	public int getAction() {
		return action;
	}

	public void setAction(int action) {
		this.action = action;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getTemplateName() {
		return templateName;
	}

	public void setTemplateName(String templateName) {
		this.templateName = templateName;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}

	public String getFolderName() {
		return folderName;
	}

	public void setFolderName(String folderName) {
		this.folderName = folderName;
	}
}