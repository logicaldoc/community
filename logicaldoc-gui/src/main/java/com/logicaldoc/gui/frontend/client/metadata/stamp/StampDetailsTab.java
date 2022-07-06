package com.logicaldoc.gui.frontend.client.metadata.stamp;

import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the stamp details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public abstract class StampDetailsTab extends HLayout {

	protected GUIStamp stamp;

	protected ChangedHandler changedHandler;

	public StampDetailsTab(GUIStamp stamp, ChangedHandler changedHandler) {
		super();
		this.stamp = stamp;
		this.changedHandler = changedHandler;
	}

	public GUIStamp getStamp() {
		return stamp;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}
}