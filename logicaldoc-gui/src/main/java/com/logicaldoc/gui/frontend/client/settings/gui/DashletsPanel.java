package com.logicaldoc.gui.frontend.client.settings.gui;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.DashletService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the dashlets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletsPanel extends VLayout {

	private static final String QUERY = "query";

	private static final String TITLE = "title";

	private static final String CONTENT = "content";

	private ListGrid grid;

	private ListGridRecord rollOverRecord;

	private HLayout rollOverCanvas;

	private List<GUIDashlet> dashlets = new ArrayList<>();

	public DashletsPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		HTMLFlow hint = new HTMLFlow(I18N.message("dashletshint"));
		hint.setMargin(3);

		ToolStripButton add = new ToolStripButton();
		add.setAutoFit(true);
		add.setTitle(I18N.message("adddashlet"));
		add.addClickHandler(event -> {
			TextItem item = ItemFactory.newSimpleTextItem("name", "", null);
			item.setRequired(true);
			LD.askForValue(I18N.message("newdashlet"), I18N.message("name"), null, item, value -> {
				GUIDashlet dashlet = new GUIDashlet();
				dashlet.setName(value);
				dashlet.setType(CONTENT);
				dashlet.setId(-1L);
				dashlets.add(dashlet);

				refreshGrid();

				new DashletEditor(dashlet, DashletsPanel.this).show();
			});
		});

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> saveDashlets());

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setAutoFitWidth(true);
		id.setRequired(true);
		id.setCanEdit(false);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(100);
		name.setRequired(true);

		ListGridField title = new ListGridField(TITLE, I18N.message(TITLE));
		title.setWidth(120);
		title.setCellFormatter((value, rec, rowNum, colNum) -> value != null ? I18N.message(value.toString()) : "");

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setWidth(125);
		type.setCellFormatter((value, rec, rowNum, colNum) -> I18N.message("dashlet.type." + value));

		ListGridField max = new ListGridField("max", I18N.message("max"));
		max.setType(ListGridFieldType.INTEGER);
		max.setWidth(50);
		max.setAlign(Alignment.CENTER);

		ListGridField content = new ListGridField(CONTENT, I18N.message(CONTENT));
		content.setWidth("*");

		ListGridField query = new ListGridField(QUERY, I18N.message(QUERY));
		query.setWidth("*");

		grid = new ListGrid() {
			@Override
			protected Canvas getRollOverCanvas(Integer rowNum, Integer colNum) {
				rollOverRecord = this.getRecord(rowNum);

				if (rollOverCanvas == null) {
					rollOverCanvas = new HLayout(3);
					rollOverCanvas.setSnapTo("R");
					rollOverCanvas.setWidth(50);
					rollOverCanvas.setHeight(22);

					ImgButton editImg = new ImgButton();
					editImg.setShowDown(false);
					editImg.setShowRollOver(false);
					editImg.setLayoutAlign(Alignment.CENTER);
					editImg.setSrc("[SKIN]/actions/edit.png");
					editImg.setPrompt(I18N.message("edit"));
					editImg.setHeight(16);
					editImg.setWidth(16);
					editImg.addClickHandler(event -> onEdit());

					rollOverCanvas.addMember(editImg);
				}
				return rollOverCanvas;

			}
		};

		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setShowAllRecords(true);
		grid.setCanEdit(false);
		grid.setWidth100();
		grid.setHeight100();
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setModalEditing(true);
		grid.setShowRollOverCanvas(true);
		grid.setShowRollUnderCanvas(false);
		grid.setFields(id, name, title, type, max, query, content);

		grid.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		grid.addDoubleClickHandler(event -> onEdit());

		setMembers(hint, toolStrip, grid);

		reload();
	}

	/**
	 * Sends the dashlets
	 */
	private void saveDashlets() {
		DashletService.Instance.get().saveDashlets(dashlets, new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void onEdit() {
		DashletEditor editor = new DashletEditor(dashlets.get(grid.getRecordIndex(rollOverRecord)), DashletsPanel.this);
		editor.show();
	}

	public void refreshGrid() {
		ListGridRecord[] records = new ListGridRecord[dashlets.size()];
		int i = 0;
		for (GUIDashlet dashlet : dashlets) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", dashlet.getId());
			rec.setAttribute("name", dashlet.getName());
			rec.setAttribute("type", dashlet.getType());
			rec.setAttribute(TITLE, dashlet.getTitle());
			rec.setAttribute("max", dashlet.getMax());
			rec.setAttribute(QUERY, dashlet.getQuery());
			rec.setAttribute(CONTENT, dashlet.getContent());
			records[i++] = rec;
		}
		grid.setData(records);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> DashletService.Instance.get()
				.delete(grid.getSelectedRecord().getAttributeAsLong("id"), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						reload();
					}
				}));

		delete.setEnabled(!GUIDashlet.isSystemDashlet(grid.getSelectedRecord().getAttributeAsString("name")));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	private void reload() {
		DashletService.Instance.get().loadDashlets(new AsyncCallback<List<GUIDashlet>>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUIDashlet> dashlts) {
				dashlets.addAll(dashlts);
				refreshGrid();
			}
		});
	}
}