package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.frontend.client.panels.DetailTab;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Superclass for all tab panels in the document details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class DocumentDetailTab extends DetailTab {
	protected GUIDocument document;

	protected boolean updateEnabled = false;

	protected boolean deleteEnabled = false;

	/**
	 * 
	 * @param document The document this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        document
	 */
	protected DocumentDetailTab(GUIDocument document, ChangedHandler changedHandler) {
		super();
		this.document = document;
		this.changedHandler = changedHandler;

		if (Session.get().isAdmin() && !document.isImmutable() && document.getStatus() == Constants.DOC_UNLOCKED) {
			updateEnabled = true;
			deleteEnabled = true;
		} else {
			updateEnabled = (!document.isImmutable() && (document.getStatus() == Constants.DOC_UNLOCKED
					|| document.getLockUserId() == Session.get().getUser().getId()) && document.isWrite());
			deleteEnabled = (!document.isImmutable() && (document.getStatus() == Constants.DOC_UNLOCKED
					|| document.getLockUserId() == Session.get().getUser().getId()) && document.isDelete());
		}
	}

	public GUIDocument getDocument() {
		return document;
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