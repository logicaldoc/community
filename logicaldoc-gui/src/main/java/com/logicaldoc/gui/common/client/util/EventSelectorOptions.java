package com.logicaldoc.gui.common.client.util;

public class EventSelectorOptions {
	
	private boolean folder;

	private boolean workflow;

	private boolean user;

	private boolean importfolder;

	private boolean ocr;

	private boolean webservice;

	private boolean all;

	private boolean ai;

	public EventSelectorOptions(EventSelectorOptionsParameter parameter) {
		this.folder = parameter.isFolder();
		this.workflow = parameter.isWorkflow();
		this.user = parameter.isUser();
		this.importfolder = parameter.isImportfolder();
		this.ocr = parameter.isOcr();
		this.webservice = parameter.isWebservice();
		this.ai = parameter.isAi();
		this.all=parameter.isAll();
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

	public boolean isAll() {
		return all;
	}

	public void setAll(boolean all) {
		this.all = all;
	}

	public boolean isAi() {
		return ai;
	}

	public void setAi(boolean ai) {
		this.ai = ai;
	}
}