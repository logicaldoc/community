package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class GUIForm extends GUIExtensibleObject implements Serializable {
	private static final long serialVersionUID = 1L;

	public static final String DEFAULT_WEBCSS = ".webform-header {\n  margin-top: 10px;\n"
			+ "  background-color: #fff;\n  border: 1px solid #dadce0;\n  border-radius: 8px;\n"
			+ "  margin-bottom: 12px;\n  padding: 24px;\n}\n.webform-title {\n"
			+ "  font-family: Roboto,Arial,sans-serif;\n  font-size: 32px; \n  font-weight: 400;\n"
			+ "  line-height: 40px;\n  color: #202124;\n  line-height: 135%;\n  max-width: 100%;\n"
			+ "  min-width: 0%;\n}\n.webform-description {\n  font-family: Roboto,Arial,sans-serif;\n"
			+ "  font-size: 14px;\n  font-weight: 400;\n  line-height: 20px;\n  color: #202124;\n"
			+ "  margin-top: 12px;\n}\n.webform-form, .webform-email {\n  margin-top: 2px;\n"
			+ "  background-color: #fff;\n  border: 1px solid #dadce0;\n  border-radius: 8px;\n"
			+ "  margin-bottom: 5px;\n  padding: 24px;\n}\n"
			+ ".webform-item label, .webform-itemFocused label, .webform-itemDisabled label{\n"
			+ "    font-family: Roboto,Arial,sans-serif;\n    font-size: 16px;\n    font-weight: 500;\n"
			+ "    letter-spacing: .1px;\n    line-height: 24px;\n    color: #202124;\n"
			+ "    font-weight: 400;\n    word-break: break-word;\n}\n.webform-footer {\n"
			+ "  font-family: Roboto,Arial,sans-serif;\n  font-size: 12px;\n  font-weight: 400;\n"
			+ "  line-height: 16px;\n  color: #70757a;\n  margin-top: 2px;\n  padding: 5px;\n}\n"
			+ ".webform-submit{\n  font-family: Roboto,Arial,sans-serif;\n  background-color: #F56C13;\n"
			+ "  color: #ffffff;\n  border: 2px solid transparent;\n  font-size: 16px;\n"
			+ "  line-height: 30px;\n  border-radius: 4px;\n}\n"
			+ ".webform-submitOver, .webform-submitFocused, .webform-submitFocusedOver {\n"
			+ "  font-family: Roboto,Arial,sans-serif;\n  background-color: #D8590A;\n  color: #ffffff;\n"
			+ "  border: 2px solid transparent;\n  font-size: 16px;\n  line-height: 30px;\n"
			+ "  border-radius: 4px;\n}\n.webform-submitDown, .webform-submitFocusedDown {\n"
			+ "  font-family: Roboto,Arial,sans-serif;\n  background-color: #D8590A;\n  color: black;\n"
			+ "  border: 2px solid transparent;\n  font-size: 16px;\n  line-height: 30px;\n"
			+ "  border-radius: 4px;\n}\n .webform-confirmation, .webform-disabled {\n"
			+ "  font-family: Roboto,Arial,sans-serif;\n  font-size: 14pt;\n}\n"
			+ ".webform-sendcopy, .webform-sendcopyOver {\n  font-family: Roboto,Arial,sans-serif;\n"
			+ "  font-size: 12px;\n}\n.contactingserver{\n  font-family: Roboto,Arial,sans-serif;\n"
			+ "  font-size: 16pt;\n  font-weight: bold;\n  white-space: nowrap;\n}";

	private String name;

	private String title;

	private String description;

	private String footer;

	/**
	 * The image to put in the header of the web form
	 */
	private String headerImage;

	/**
	 * The body used to render the final document
	 */
	private String content;

	/**
	 * Special CSS to use to render the content
	 */
	private String css;

	private boolean webEnabled = false;

	private boolean collectEmails = true;

	private boolean sendCopy = true;

	private String backgroundColor = "#F8F4F4";

	/**
	 * An alternative unique identifier of the form
	 */
	private String formId;

	private int width = 500;

	private int columns = 1;

	/**
	 * The CSS to use in the web form
	 */
	private String webCss = null;

	private GUIFolder targetFolder;

	private String languageForDoc;

	private String locale;

	private String confirmation;

	private String responderEmail;

	private boolean sendResponses = true;

	private boolean editAfterSubmit = true;

	private boolean notifyResponses = false;

	private Long editingDocId;

	private int statChartWidth = 250;

	private GUIUser[] recipients;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getHeaderImage() {
		return headerImage;
	}

	public void setHeaderImage(String headerImage) {
		this.headerImage = headerImage;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public String getCss() {
		return css;
	}

	public void setCss(String css) {
		this.css = css;
	}

	public String getFormId() {
		return formId;
	}

	public void setFormId(String formId) {
		this.formId = formId;
	}

	public boolean isWebEnabled() {
		return webEnabled;
	}

	public void setWebEnabled(boolean webEnabled) {
		this.webEnabled = webEnabled;
	}

	public String getBackgroundColor() {
		return backgroundColor;
	}

	public void setBackgroundColor(String backgroundColor) {
		this.backgroundColor = backgroundColor;
	}

	public int getWidth() {
		return width;
	}

	public void setWidth(int width) {
		this.width = width;
	}

	public String getWebCss() {
		return webCss;
	}

	public void setWebCss(String webCss) {
		this.webCss = webCss;
	}

	public String getFooter() {
		return footer;
	}

	public void setFooter(String footer) {
		this.footer = footer;
	}

	public int getColumns() {
		return columns;
	}

	public void setColumns(int columns) {
		this.columns = columns;
	}

	public GUIFolder getTargetFolder() {
		return targetFolder;
	}

	public void setTargetFolder(GUIFolder targetFolder) {
		this.targetFolder = targetFolder;
	}

	public String getLanguageForDoc() {
		return languageForDoc;
	}

	public void setLanguageForDoc(String languageForDoc) {
		this.languageForDoc = languageForDoc;
	}

	public String getConfirmation() {
		return confirmation;
	}

	public void setConfirmation(String confirmation) {
		this.confirmation = confirmation;
	}

	public boolean isCollectEmails() {
		return collectEmails;
	}

	public void setCollectEmails(boolean collectEmails) {
		this.collectEmails = collectEmails;
	}

	public String getResponderEmail() {
		return responderEmail;
	}

	public void setResponderEmail(String responderEmail) {
		this.responderEmail = responderEmail;
	}

	public boolean isSendCopy() {
		return sendCopy;
	}

	public void setSendCopy(boolean sendCopy) {
		this.sendCopy = sendCopy;
	}

	public boolean isSendResponses() {
		return sendResponses;
	}

	public void setSendResponses(boolean sendResponses) {
		this.sendResponses = sendResponses;
	}

	public String getLocale() {
		return locale;
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}

	public boolean isEditAfterSubmit() {
		return editAfterSubmit;
	}

	public void setEditAfterSubmit(boolean editAfterSubmit) {
		this.editAfterSubmit = editAfterSubmit;
	}

	public Long getEditingDocId() {
		return editingDocId;
	}

	public void setEditingDocId(Long editingDocId) {
		this.editingDocId = editingDocId;
	}

	public int getStatChartWidth() {
		return statChartWidth;
	}

	public void setStatChartWidth(int statChartWidth) {
		this.statChartWidth = statChartWidth;
	}

	public boolean isNotifyResponses() {
		return notifyResponses;
	}

	public void setNotifyResponses(boolean notifyResponses) {
		this.notifyResponses = notifyResponses;
	}

	public GUIUser[] getRecipients() {
		return recipients;
	}

	public void setRecipients(GUIUser[] recipients) {
		this.recipients = recipients;
	}
}