package com.logicaldoc.gui.frontend.client.impex.syndication;

import com.logicaldoc.gui.common.client.beans.GUISyndication;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the syndication details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public abstract class SyndicationDetailsTab extends HLayout {
	protected GUISyndication syndication;

	protected ChangedHandler changedHandler;

	/**
	 * 
	 * @param syndication The syndication this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        syndication
	 */
	protected SyndicationDetailsTab(GUISyndication syndication, ChangedHandler changedHandler) {
		super();
		this.syndication = syndication;
		this.changedHandler = changedHandler;
	}

	public GUISyndication getSyndication() {
		return syndication;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
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