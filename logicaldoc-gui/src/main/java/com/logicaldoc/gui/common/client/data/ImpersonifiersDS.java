package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;

/**
 * Datasource to retrieve all users. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImpersonifiersDS extends UsersDS {
	public ImpersonifiersDS() {
		this(Session.get().getUser().getId());
	}

	public ImpersonifiersDS(long userId) {
		super(null, true, false);

		super.getDataURL();
		setDataURL(super.getDataURL() + "&impersonifiers=" + userId);
	}
}