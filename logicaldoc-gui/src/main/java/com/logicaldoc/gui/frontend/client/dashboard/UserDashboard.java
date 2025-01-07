package com.logicaldoc.gui.frontend.client.dashboard;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.dashboard.dashlet.Dashlet;
import com.logicaldoc.gui.frontend.client.dashboard.dashlet.DashletSelector;
import com.logicaldoc.gui.frontend.client.services.DashletService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.layout.PortalLayout;
import com.smartgwt.client.widgets.layout.Portlet;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * User dashboard that displays several portlets like a portal page.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UserDashboard extends VLayout {

	private static UserDashboard instance;

	private PortalLayout portal = null;

	public UserDashboard() {
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton addDashlet = new ToolStripButton();
		addDashlet.setTitle(I18N.message("adddashlet"));
		toolStrip.addButton(addDashlet);
		addDashlet.addClickHandler(event -> {
			DashletSelector selector = new DashletSelector(portal);
			selector.show();
		});

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		toolStrip.addButton(save);
		save.addClickHandler(event -> save());

		toolStrip.addFill();

		portal = new PortalLayout();
		portal.setShowColumnMenus(false);
		portal.setShowEdges(false);
		portal.setShowShadow(false);
		portal.setWidth100();
		portal.setHeight100();
		portal.setColumnBorder("0px");
		portal.setColumnOverflow(Overflow.AUTO);
		portal.setOverflow(Overflow.VISIBLE);

		int maxCol = 0;
		int maxRow = 0;
		int maxIndex = 0;

		for (GUIDashlet gd : Session.get().getUser().getDashlets()) {
			if (maxCol < gd.getColumn())
				maxCol = gd.getColumn();
			if (maxRow < gd.getRow())
				maxRow = gd.getRow();
			if (maxIndex < gd.getIndex())
				maxIndex = gd.getIndex();
		}

		putPortlets(maxCol, maxRow, maxIndex);

		addMember(toolStrip);
		addMember(portal);
	}

	private void putPortlets(int maxCol, int maxRow, int maxIndex) {
		Dashlet[][][] portlets = new Dashlet[maxCol + 1][maxRow + 1][maxIndex + 1];
		for (GUIDashlet gd : Session.get().getUser().getDashlets())
			portlets[gd.getColumn()][gd.getRow()][gd.getIndex()] = Dashlet.getDashlet(gd);

		for (int col = 0; col <= maxCol; col++)
			for (int row = 0; row <= maxRow; row++)
				for (int index = 0; index <= maxIndex; index++)
					if (portlets[col][row][index] != null)
						portal.addPortlet(portlets[col][row][index], col, row, index);
	}

	public static UserDashboard get() {
		if (instance == null)
			instance = new UserDashboard();
		return instance;
	}

	/**
	 * Persistently saves the portal layout
	 */
	public void save() {
		Portlet[][][] portlets = portal.getPortletArray();

		List<GUIDashlet> dashlets = new ArrayList<>();

		for (int column = 0; column < portlets.length; column++)
			for (int row = 0; row < portlets[column].length; row++)
				for (int i = 0; i < portlets[column][row].length; i++) {
					Dashlet dashlet = (Dashlet) portlets[column][row][i];
					GUIDashlet guiDashlet = dashlet.getGuiDashlet();
					guiDashlet.setColumn(column);
					guiDashlet.setRow(row);
					guiDashlet.setIndex(i);
					dashlets.add(guiDashlet);
				}

		Session.get().getUser().setDashlets(dashlets);
		DashletService.Instance.get().saveUserDashlets(dashlets, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void ret) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((portal == null) ? 0 : portal.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		UserDashboard other = (UserDashboard) obj;
		if (portal == null) {
			if (other.portal != null)
				return false;
		} else if (!portal.equals(other.portal))
			return false;
		return true;
	}
}