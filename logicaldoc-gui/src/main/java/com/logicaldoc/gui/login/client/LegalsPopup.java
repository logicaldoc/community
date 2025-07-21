package com.logicaldoc.gui.login.client;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to show the legals to confirm.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.2
 */
public class LegalsPopup extends Window {

	private LegalPreview previewPanel = null;

	private HLayout layout = new HLayout();

	private VLayout previewSlot = new VLayout();

	private String username;

	/**
	 * Gallery of legals to navigate
	 */
	private Map<String, GUIParameter> legals = new HashMap<>();

	private GUIParameter currentLegal;
	
	LegalsPopup(String username, List<GUIParameter> legals) {
		this.username = username;
		for (GUIParameter legal : legals)
			this.legals.put(legal.getName(), legal);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setWidth(Math.round((float) com.google.gwt.user.client.Window.getClientWidth() - 20));
		setHeight(Math.round((float) com.google.gwt.user.client.Window.getClientHeight() - 20));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMargin(2);

		addCloseClickHandler(event -> {
			if (previewPanel != null)
				previewPanel.destroy();
			destroy();
		});

		previewSlot.setWidth100();
		previewSlot.setHeight100();

		layout.setMembers(previewSlot);

		addItem(layout);

		layout.setMembers(previewSlot);

		currentLegal=legals.get(0);
		reloadPreview();
	}

	private void reloadPreview() {
		setTitle(I18N.message("preview") + " - " + currentLegal.getValue());

		if (previewPanel != null)
			previewSlot.removeMember(previewPanel);

		previewPanel = new LegalPreview(username, currentLegal, click -> onLegalConfirmed());
		previewPanel.setWidth100();
		previewPanel.setHeight100();

		previewSlot.setMembers(previewPanel);
	}

	protected void onLegalConfirmed() {
		legals.remove(currentLegal.getName());
		if(legals.isEmpty()) {
			SC.say(I18N.message("thanksconfirmlegal"));
			destroy();
		} else {
			currentLegal = legals.values().iterator().next();
			reloadPreview();
		}
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}