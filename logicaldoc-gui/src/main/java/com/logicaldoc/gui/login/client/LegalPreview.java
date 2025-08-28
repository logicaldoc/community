package com.logicaldoc.gui.login.client;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel is used to show the preview of a legal document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class LegalPreview extends VLayout {

	protected HTMLPane preview = null;

	protected GUIParameter legal;

	protected String username;

	protected ClickHandler confirmation;

	protected int width;

	protected int height;

	protected ToolStrip topConfirmReadingToolStrip = new ToolStrip();

	protected ToolStripButton topConfirmReadingButton = new ToolStripButton(I18N.message("confirmlegal"));

	protected ToolStrip bottomConfirmReadingToolStrip = new ToolStrip();

	protected ToolStripButton bottomConfirmReadingButton = new ToolStripButton(I18N.message("confirmlegal"));

	public LegalPreview(String username, GUIParameter legal, ClickHandler confirmation) {
		this.legal = legal;
		this.username = username;
		this.confirmation = confirmation;

		declareOnReadingCompleted(this);
	}

	@Override
	protected void onDraw() {
		addTopConfirmReadingPanel();

		addPreview();

		addBottomConfirmReadingPanel();
	}

	private void confirmReading() {
		LoginService.Instance.get().confirmLegal(username, legal.getName(), new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void v) {
				topConfirmReadingToolStrip.removeMember(topConfirmReadingButton);
				confirmation.onClick(null);
			}
		});
	}

	private void addPreview() {
		preview = new HTMLPane();
		preview.setWidth100();
		preview.setHeight100();
		preview.setContentsType(ContentsType.PAGE);
		preview.setContentsURL(legalPreviewUrl());
		addMember(preview);
	}

	protected String legalPreviewUrl() {
		return Util.contextPath() + "prev/legal.jsp?legal=" + legal.getName() + "&username=" + username + "&locale="
				+ I18N.getLocale() + "#locale=" + I18N.getLocale().replace('_', '-');
	}

	public void onReadingCompleted() {
		topConfirmReadingButton.setDisabled(false);
		topConfirmReadingButton.setTooltip("");
		topConfirmReadingButton.setStyleName("btn");
		bottomConfirmReadingButton.setDisabled(false);
		bottomConfirmReadingButton.setTooltip("");
		bottomConfirmReadingButton.setStyleName("btn");
	}

	private void addTopConfirmReadingPanel() {
		topConfirmReadingToolStrip.setWidth100();
		topConfirmReadingToolStrip.setAlign(Alignment.RIGHT);

		topConfirmReadingButton.addClickHandler(click -> confirmReading());
		topConfirmReadingButton.setDisabled(true);
		topConfirmReadingButton.setTooltip(I18N.message("readalldoctoenablebutton"));

		Label topConfirmReadingLabel = new Label(I18N.message("yourequiredtoconfirmlegal", legal.getValue()));
		topConfirmReadingLabel.setWrap(false);
		topConfirmReadingLabel.setAlign(Alignment.LEFT);

		topConfirmReadingToolStrip.addMember(topConfirmReadingLabel);
		topConfirmReadingToolStrip.addFill();
		topConfirmReadingToolStrip.addButton(topConfirmReadingButton);

		addMember(topConfirmReadingToolStrip);
	}

	private void addBottomConfirmReadingPanel() {
		bottomConfirmReadingToolStrip.setWidth100();
		bottomConfirmReadingToolStrip.setAlign(Alignment.RIGHT);

		bottomConfirmReadingButton.addClickHandler(click -> confirmReading());
		bottomConfirmReadingButton.setDisabled(true);
		bottomConfirmReadingButton.setTooltip(I18N.message("readalldoctoenablebutton"));

		Label bottomConfirmReadingLabel = new Label(I18N.message("yourequiredtoconfirmlegal", legal.getValue()));
		bottomConfirmReadingLabel.setWrap(false);
		bottomConfirmReadingLabel.setAlign(Alignment.LEFT);

		bottomConfirmReadingToolStrip.addMember(bottomConfirmReadingLabel);
		bottomConfirmReadingToolStrip.addFill();
		bottomConfirmReadingToolStrip.addButton(bottomConfirmReadingButton);

		addMember(bottomConfirmReadingToolStrip);
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	/**
	 * Declares the javascript function used to notify when the document has
	 * been completely read
	 * 
	 * @param previewPanel the preview panel
	 */
	public static native void declareOnReadingCompleted(LegalPreview previewPanel) /*-{
		$wnd.onReadingCompleted = function() {
			return previewPanel.@com.logicaldoc.gui.login.client.LegalPreview::onReadingCompleted()();
		};
	}-*/;
}