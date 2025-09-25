package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.Offline;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a preview panels in the Documents workspace
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class DocumentsPreviewPanel extends VLayout {

	private Canvas child;

	protected String selectLabelString = I18N.message("selectadocument");

	protected Label selectLabel;

	protected String widthCookieName;

	public DocumentsPreviewPanel() {
		this(CookiesManager.COOKIE_DOCSLIST_PREV_W);
	}

	public DocumentsPreviewPanel(String widthCookieName) {
		this.widthCookieName = widthCookieName;
		setAlign(Alignment.CENTER);
		selectLabel = new Label("&nbsp;" + selectLabelString);
		selectLabel.setAlign(Alignment.CENTER);
		selectLabel.setOverflow(Overflow.HIDDEN);

		setInitialSize();
		reset();

		addResizedHandler(event -> {
				if (getWidth() > 1)
					Offline.put(widthCookieName, getWidthAsString());
		});
	}

	public void setDocument(GUIDocument document) {
		if (isVisible() && getWidth() > 10 && document != null) {
			if (child != null) {
				removeMember(child);
				child.destroy();
			}

			child = new com.logicaldoc.gui.common.client.preview.PreviewPanel(document);
			addMember(child);
		} else {
			reset();
		}
	}

	public void reset() {
		if (child != null) {
			removeMember(child);
			child.destroy();
		}

		child = selectLabel;
		addMember(child);
	}

	protected void setInitialSize() {
		if (!Session.get().getConfigAsBoolean("gui.preview.openpanel")) {
			setWidth(0);
		} else {
			try {
				// Retrieve the saved preview width
				String w = (String) Offline.get(widthCookieName);
				if (w == null || w.isEmpty())
					w = "xxxx";
				setWidth(Integer.parseInt(w));
			} catch (Exception t) {
				setWidth(350);
			}
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