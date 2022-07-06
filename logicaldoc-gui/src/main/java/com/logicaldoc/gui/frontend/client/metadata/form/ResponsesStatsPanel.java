package com.logicaldoc.gui.frontend.client.metadata.form;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.data.AttributesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tile.TileGrid;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import com.smartgwt.client.widgets.viewer.DetailFormatter;
import com.smartgwt.client.widgets.viewer.DetailViewerField;

/**
 * This panel shows a list of responses.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ResponsesStatsPanel extends VLayout {

	private TileGrid grid;

	private GUIForm form;

	public ResponsesStatsPanel(GUIForm form) {
		this.form = form;
	}

	@Override
	protected void onDraw() {
		if (form.getId() == 0L)
			return;

		SpinnerItem tileWidth = ItemFactory.newSpinnerItem("tilesize", "tilesize", form.getStatChartWidth(), 150, null);
		tileWidth.setStep(10);
		tileWidth.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh((Integer) tileWidth.getValue());
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (tileWidth.validate()) {
					form.setStatChartWidth((Integer) tileWidth.getValue());
					FormService.Instance.get().save(form, new AsyncCallback<GUIForm>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIForm frm) {
							form.setId(frm.getId());
							refresh(form.getStatChartWidth());
						}
					});
				}
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addFormItem(tileWidth);
		toolStrip.addButton(save);

		addMember(toolStrip);
		refresh(form.getStatChartWidth());
	}

	private void refresh(int tileWidth) {
		if (grid != null)
			removeMember(grid);

		grid = new TileGrid();
		grid.setEmptyMessage(I18N.message("nostastoshow"));
		grid.setTileWidth(tileWidth);
		grid.setTileHeight((int) ((double) tileWidth * 1.25d));
		grid.setAutoFetchData(true);
		grid.setSelectionType(SelectionStyle.MULTIPLE);
		grid.setShowAllRecords(false);
		grid.setCanReorderTiles(false);
		grid.setCanDrag(false);
		grid.setWidth100();

		grid.setDataSource(new AttributesDS(form.getTemplateId()));

		DetailViewerField chart = new DetailViewerField("name");
		chart.setDetailFormatter(new DetailFormatter() {

			@Override
			public String format(Object value, Record record, DetailViewerField field) {
				if (value == null)
					return "";

				String attributeName = value.toString();
				if (attributeName.startsWith("ext_"))
					attributeName = attributeName.substring(4);

				String chartUrl = Util.contextPath() + "formchart?formId=" + form.getId() + "&attribute="
						+ attributeName + "&locale=" + I18N.getLocale() + "&width=" + tileWidth + "&random="
						+ new Date().getTime();
				return "<img border='0' align='absmidle' alt='" + value + "' title='" + value + "' src='" + chartUrl
						+ "' />";
			}
		});
		grid.setFields(chart);

		addMember(grid);
	}
}