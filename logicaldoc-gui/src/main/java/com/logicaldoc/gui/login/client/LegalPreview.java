package com.logicaldoc.gui.login.client;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.HTMLFlow;
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

	protected HTMLFlow preview = null;

	protected GUIParameter legal;

	protected String username;

	protected ClickHandler confirmation;

	protected int width;

	protected int height;

	protected ToolStrip topConfirmReadingToolStrip = new ToolStrip();

	protected ToolStripButton topConfirmReadingButton = new ToolStripButton(I18N.message("confirmlegal"));

	public LegalPreview(String username, GUIParameter legal, ClickHandler confirmation) {
		this.legal = legal;
		this.username = username;
		this.confirmation = confirmation;

		declareOnReadingCompleted(this);
	}

	@Override
	protected void onDraw() {
		addTopConfirmReadingPanel();
		reloadPreview();
	}

	private void confirmReading() {
		topConfirmReadingToolStrip.removeMember(topConfirmReadingButton);
		LoginService.Instance.get().confirmLegal(username, legal.getName(), new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void v) {
				confirmation.onClick(null);
			}
		});
	}

	/**
	 * Reloads a preview.
	 */
	protected void reloadPreview() {
		if (preview == null) {
			preview = new HTMLFlow();
			preview.setWidth100();
			preview.setHeight100();
			addMember(preview);
		}

		preview.setContents("<iframe src='" + legalPreviewUrl()
				+ "' style='border:0px solid white; overflow:hidden; display: block; height: 100vh; width: 100vw;' scrolling='no' seamless='seamless'></iframe>");
		preview.redraw();
	}

	protected String legalPreviewUrl() {
		return Util.contextPath() + "prev/legal.jsp?legal=" + legal.getName() + "&username=" + username + "&locale="
				+ I18N.getLocale() + "#locale=" + I18N.getLocale().replace('_', '-');
	}

	public void onReadingCompleted() {
		topConfirmReadingButton.setDisabled(false);
		topConfirmReadingButton.setTooltip("");
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