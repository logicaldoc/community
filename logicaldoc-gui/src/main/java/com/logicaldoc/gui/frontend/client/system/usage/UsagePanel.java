package com.logicaldoc.gui.frontend.client.system.usage;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

/**
 * This panel shows the system usage grid
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class UsagePanel extends VLayout {

	private SystemUsageGrid grid;

	public UsagePanel() {
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		if (Session.get().isDefaultTenant()) {
			SelectItem tenant = ItemFactory.newTenantSelector(true);
			tenant.setRequired(true);
			tenant.setValue("-1");
			tenant.setDefaultValue("-1");
			tenant.addChangedHandler(event -> {
				removeMember(grid);
				redrawGrid(Long.parseLong(event.getValue().toString()));
			});

			ToolStrip toolStrip = new ToolStrip();
			toolStrip.addFormItem(tenant);
			addMember(toolStrip);
			redrawGrid(-1L);
		} else {
			redrawGrid(Session.get().getTenantId());
		}
	}

	private void redrawGrid(long tenantId) {
		grid = new SystemUsageGrid(true, tenantId);
		addMember(grid);
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