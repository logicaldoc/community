package com.logicaldoc.gui.frontend.client.metadata.stamp;

import java.util.Date;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.data.StampsDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of stamps
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampsPanel extends AdminPanel {
	private static final String ENABLED = "eenabled";

	private static final String IMAGE = "image";

	private static final String DESCRIPTION = "description";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_STAMP;

	static final Canvas SELECT_STAMP = new HTMLPanel("&nbsp;" + I18N.message("selectstamp"));

	public StampsPanel() {
		super("stamps");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");
		Layout listing = new VLayout();

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("60%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"), 150);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 200);

		ListGridField text = new ListGridField("text", I18N.message("text"), 200);

		ListGridField image = new ListGridField(IMAGE, I18N.message(IMAGE), 300);
		image.setCanFilter(false);
		image.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (value != null && !value.toString().trim().equals(""))
				return "<img height='60px' src='" + Util.contextPath() + "stampimage/" + value + "?random="
						+ new Date().getTime() + "'/>";
			else
				return "";
		});

		ListGridField enabled = new EnabledListGridField();

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, name, description, text, image);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setCanSort(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new StampsDS(null, false));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> refresh());

		ToolStripButton addStamp = new ToolStripButton();
		addStamp.setTitle(I18N.message("addstamp"));
		toolStrip.addButton(addStamp);
		addStamp.addClickHandler(event -> {
			list.deselectAllRecords();
			GUIStamp stamp = new GUIStamp();
			showStampDetails(stamp);
		});

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				StampService.Instance.get().getStamp(Long.parseLong(rec.getAttributeAsString("id")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(GUIStamp stamp) {
								showStampDetails(stamp);
							}
						});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showstamps", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	public void refresh() {
		list.refresh(new StampsDS(null, false));
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_STAMP;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				StampService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showStampDetails(null);
					}
				});
			}
		}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.setEnabled(!rec.getAttributeAsBoolean(ENABLED));
		enable.addClickHandler(event -> StampService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.setEnabled(rec.getAttributeAsBoolean(ENABLED));
		disable.addClickHandler(event -> StampService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		contextMenu.setItems(enable, disable, delete);
		contextMenu.showContextMenu();
	}

	public void showStampDetails(GUIStamp stamp) {
		if (!(details instanceof StampDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new StampDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((StampDetailsPanel) details).setStamp(stamp);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param stamp the stamp object
	 */
	public void updateRecord(GUIStamp stamp) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, stamp.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", stamp.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", stamp.getName());
		rec.setAttribute(DESCRIPTION, "" + stamp.getDescription());
		if (stamp.getType() == GUIStamp.TYPE_IMAGE) {
			rec.setAttribute(IMAGE, "" + stamp.getId());
			rec.setAttribute("text", "");
		} else {
			rec.setAttribute("text", "" + stamp.getText());
			rec.setAttribute(IMAGE, "");
		}
		list.refreshRow(list.getRecordIndex(rec));
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