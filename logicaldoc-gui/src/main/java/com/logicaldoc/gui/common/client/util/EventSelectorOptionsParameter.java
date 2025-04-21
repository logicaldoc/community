package com.logicaldoc.gui.common.client.util;

public class EventSelectorOptionsParameter {
	private boolean folder;

	private boolean workflow;

	private boolean user;

	private boolean importfolder;

	private boolean ocr;

	private boolean webservice;

	private boolean ai;

	private boolean all;
	
	public EventSelectorOptionsParameter(boolean folder, boolean workflow, boolean user, boolean importfolder,
			boolean ocr, boolean webservice, boolean ai) {
		this.folder = folder;
		this.workflow = workflow;
		this.user = user;
		this.importfolder = importfolder;
		this.ocr = ocr;
		this.webservice = webservice;
		this.ai = ai;
	}

	public EventSelectorOptionsParameter(boolean allFlagsTrue) {
		this(true, true, true, true, true, true, true);
		this.all = allFlagsTrue;
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

	public boolean isAi() {
		return ai;
	}

	public void setAi(boolean ai) {
		this.ai = ai;
	}

	public boolean isAll() {
		return all;
	}

	public void setAll(boolean all) {
		this.all = all;
	}
}