package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the document details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class DocumentDetailTab extends HLayout {
	protected GUIDocument document;

	protected ChangedHandler changedHandler;

	protected boolean updateEnabled = false;

	protected boolean deleteEnabled = false;

	/**
	 * 
	 * @param document The document this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        document
	 */
	public DocumentDetailTab(GUIDocument document, ChangedHandler changedHandler) {
		super();
		this.document = document;
		this.changedHandler = changedHandler;

		if (Session.get().isAdmin() && document.getImmutable() == 0 && document.getStatus() == Constants.DOC_UNLOCKED) {
			updateEnabled = true;
			deleteEnabled = true;
		} else {
			updateEnabled = (document.getImmutable() == 0
					&& (document.getStatus() == Constants.DOC_UNLOCKED
							|| document.getLockUserId() == Session.get().getUser().getId())
					&& document.getFolder().isWrite());
			deleteEnabled = (document.getImmutable() == 0
					&& (document.getStatus() == Constants.DOC_UNLOCKED
							|| document.getLockUserId() == Session.get().getUser().getId())
					&& document.getFolder().isDelete());
		}

	}

	public GUIDocument getDocument() {
		return document;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}

	public boolean validate() {
		return true;
	}

	public void handleErrors(ServerValidationException errorException) {
		GuiLog.serverError(errorException);
		handleErrors(errorException.getErrors());
	}

	public void handleErrors(ServerValidationError[] errors) {

	}
}
