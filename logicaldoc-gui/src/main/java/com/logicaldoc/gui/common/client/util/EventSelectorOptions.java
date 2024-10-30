package com.logicaldoc.gui.common.client.util;

public class EventSelectorOptions {
	private boolean folder;

	private boolean workflow;

	private boolean user;

	private boolean importfolder;

	private boolean ocr;

	private boolean webservice;

	private boolean allOption;

	public EventSelectorOptions(boolean folder, boolean workflow, boolean user, boolean importfolder, boolean ocr,
			boolean webservice, boolean allOption) {
		this.folder = folder;
		this.workflow = workflow;
		this.user = user;
		this.importfolder = importfolder;
		this.ocr = ocr;
		this.webservice = webservice;
		this.allOption = allOption;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public boolean isWorkflow() {
		return workflow;
	}

	public void setWorkflow(boolean workflow) {
		this.workflow = workflow;
	}

	public boolean isUser() {
		return user;
	}

	public void setUser(boolean user) {
		this.user = user;
	}

	public boolean isImportfolder() {
		return importfolder;
	}

	public void setImportfolder(boolean importfolder) {
		this.importfolder = importfolder;
	}

	public boolean isOcr() {
		return ocr;
	}

	public void setOcr(boolean ocr) {
		this.ocr = ocr;
	}

	public boolean isWebservice() {
		return webservice;
	}

	public void setWebservice(boolean webservice) {
		this.webservice = webservice;
	}

	public boolean isAllOption() {
		return allOption;
	}

	public void setAllOption(boolean allOption) {
		this.allOption = allOption;
	}
}