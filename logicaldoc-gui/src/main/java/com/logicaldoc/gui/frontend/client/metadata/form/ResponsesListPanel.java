package com.logicaldoc.gui.frontend.client.metadata.form;

import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of responses.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ResponsesListPanel extends VLayout {

	private ResponsesGrid grid;

	private GUIForm form;

	public ResponsesListPanel(GUIForm form) {
		this.form = form;
	}

	public void refresh(int max) {
		ResponsesDS dataSource = new ResponsesDS(form, max);
		grid.refresh(dataSource);
	}

	@Override
	protected void onDraw() {
		final SpinnerItem max = ItemFactory.newSpinnerItem("max", "", 100, 5, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setStep(10);
		max.addChangedHandler(changed -> refresh((Integer) max.getValue()));

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		display.addClickHandler(click -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh((Integer) max.getValue());
		});
		toolStrip.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		toolStrip.addButton(export);
		export.addClickHandler(click -> GridUtil.exportCSV(grid, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		toolStrip.addButton(print);
		print.addClickHandler(click -> GridUtil.print(grid));

		// Prepare a panel containing a title and the documents number
		final InfoPanel infoPanel = new InfoPanel("");

		grid = new ResponsesGrid(form);
		grid.setDataSource(new ResponsesDS(form, 100));
		grid.addDataArrivedHandler(dataArrived -> infoPanel
				.setMessage(I18N.message("shownresponses", Integer.toString(grid.getTotalRows()))));

		setMembers(toolStrip, infoPanel, grid);
	}
}