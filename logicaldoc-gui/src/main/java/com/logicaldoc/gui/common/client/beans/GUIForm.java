package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Representation of a form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class GUIForm extends GUIExtensibleObject implements Serializable {
	private static final long serialVersionUID = 1L;

	public static final String DEFAULT_WEBCSS = """
			.webform-header {
			  margin-top: 10px;
			  background-color: #fff;
			  border: 1px solid #dadce0;
			  border-radius: 8px;
			  margin-bottom: 12px;
			  padding: 24px;
			}

			.webform-title {
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 32px; 
			  font-weight: 400;
			  line-height: 40px;
			  color: #202124;
			  line-height: 135%;
			  max-width: 100%;
			  min-width: 0%;
			}

			.webform-description {
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 14px;
			  font-weight: 400;
			  line-height: 20px;
			  color: #202124;
			  margin-top: 12px;
			}

			.webform-form, .webform-email {
			  margin-top: 2px;
			  background-color: #fff;
			  border: 1px solid #dadce0;
			  border-radius: 8px;
			  margin-bottom: 5px;
			  padding: 24px;
			}

			.webform-item label, .webform-itemFocused label, .webform-itemDisabled label{
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 16px;
			  font-weight: 500;
			  letter-spacing: .1px;
			  line-height: 24px;
			  color: #202124;
			  font-weight: 400;
			  word-break: break-word;
			}

			.webform-footer {
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 12px;
			  font-weight: 400;
			  line-height: 16px;
			  color: #70757a;
			  margin-top: 2px;
			  padding: 5px;
			}

			.webform-submit{
			  font-family: Roboto,Arial,sans-serif;
			  background-color: #F56C13;
			  color: #ffffff;
			  border: 2px solid transparent;
			  font-size: 16px;
			  line-height: 30px;
			  border-radius: 4px;
			}

			.webform-submitOver, .webform-submitFocused, .webform-submitFocusedOver {
			  font-family: Roboto,Arial,sans-serif;
			  background-color: #D8590A;
			  color: #ffffff;
			  border: 2px solid transparent;
			  font-size: 16px;
			  line-height: 30px;
			  border-radius: 4px;
			}

			.webform-submitDown, .webform-submitFocusedDown {
			  font-family: Roboto,Arial,sans-serif;
			  background-color: #D8590A;
			  color: black;
			  border: 2px solid transparent;
			  font-size: 16px;
			  line-height: 30px;
			  border-radius: 4px;
			}

			.webform-confirmation, .webform-disabled {
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 14pt;
			}

			.webform-sendcopy, .webform-sendcopyOver {
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 12px;
			}

			.contactingserver{
			  font-family: Roboto,Arial,sans-serif;
			  font-size: 16pt;
			  font-weight: bold;
			  white-space: nowrap;
			}""";

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

	private List<GUIUser> recipients = new ArrayList<>();

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

	public List<GUIUser> getRecipients() {
		return recipients;
	}

	public void setRecipients(List<GUIUser> recipients) {
		this.recipients = recipients;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		GUIForm other = (GUIForm) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}