package com.logicaldoc.gui.common.client.widgets.preview;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to show the document preview.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class PreviewPopup extends Window {

	private PreviewPanel previewPanel = null;

	private HLayout layout = new HLayout();

	private VLayout previewSlot = new VLayout();

	private int currentIndex = 1;

	/**
	 * Gallery of documents to navigate
	 */
	private List<GUIDocument> docs = new ArrayList<>();

	private PreviewPopup() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		int size = 100;
		try {
			size = Integer.parseInt(Session.get().getInfo().getConfig("gui.preview.size"));
			if (size <= 0 || size > 100)
				size = 100;
		} catch (Exception t) {
			// Nothing to do
		}

		setWidth(Math.round((float) com.google.gwt.user.client.Window.getClientWidth() * (float) size / 100F));
		setHeight(Math.round((float) com.google.gwt.user.client.Window.getClientHeight() * (float) size / 100F));
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
	}

	/**
	 * Constructor to display a gallery of documents
	 * 
	 * @param docs the documents of the gallery
	 * @param index the index of the file to preview
	 */
	public PreviewPopup(List<GUIDocument> docs, int index) {
		this();
		this.currentIndex = index;
		this.docs = docs;

		IButton prev = new IButton("");
		prev.setWidth(35);
		prev.setHeight100();
		prev.setShowRollOver(true);
		prev.setShowDisabled(true);
		prev.setShowDown(true);
		prev.setIcon("[SKIN]/headerIcons/arrow_left.png");
		prev.addClickHandler(event -> {
			if (currentIndex <= 0)
				currentIndex = PreviewPopup.this.docs.size() - 1;
			else
				currentIndex--;
			reloadPreview(PreviewPopup.this.docs.get(currentIndex));
		});

		IButton next = new IButton("");
		next.setWidth(35);
		next.setHeight100();
		next.setShowRollOver(true);
		next.setShowDisabled(true);
		next.setShowDown(true);
		next.setIcon("[SKIN]/headerIcons/arrow_right.png");
		next.addClickHandler(event -> {
			if (currentIndex >= PreviewPopup.this.docs.size())
				currentIndex = 0;
			else
				currentIndex++;
			reloadPreview(PreviewPopup.this.docs.get(currentIndex));
		});

		layout.setMembers(prev, previewSlot, next);

		reloadPreview(docs.get(currentIndex));
	}

	/**
	 * Constructor to display a single document
	 * 
	 * @param doc the document to preview
	 */
	public PreviewPopup(GUIDocument doc) {
		this();
		setTitle(I18N.message("preview") + " - " + doc.getFileName()
				+ (doc.getFileVersion() != null ? " v" + doc.getFileVersion() : ""));
		reloadPreview(doc);
	}

	private void reloadPreview(GUIDocument doc) {
		setTitle(I18N.message("preview") + " - " + doc.getFileName()
				+ (doc.getFileVersion() != null ? " v" + doc.getFileVersion() : ""));

		if (previewPanel != null)
			previewSlot.removeMember(previewPanel);

		previewPanel = new PreviewPanel(doc);
		previewPanel.setWidth100();
		previewPanel.setHeight100();

		previewSlot.setMembers(previewPanel);
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
}