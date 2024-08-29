package com.logicaldoc.gui.frontend.client.impex.syndication;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISyndication;
import com.logicaldoc.gui.common.client.data.SyndicationsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SyndicationService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
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
 * Panel showing the details of an import folder
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SyndicationsPanel extends AdminPanel {

	private static final String EENABLED = "eenabled";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FOLDER;

	static final Canvas SELECT_FOLDER = new HTMLPanel("&nbsp;" + I18N.message("selectsyndication"));

	public SyndicationsPanel() {
		super("syndication");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("60%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 100);
		name.setCanFilter(true);

		ListGridField url = new ListGridField("url", I18N.message("remoteurl"), 300);
		url.setCanFilter(true);

		ListGridField targetPath = new ListGridField("targetPath", I18N.message("targetpath"), 300);
		targetPath.setCanFilter(true);

		ListGridField enabled = new ListGridField(EENABLED, " ", 30);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, name, url, targetPath);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new SyndicationsDS());

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> refresh());

		ToolStripButton addSyndication = new ToolStripButton();
		addSyndication.setTitle(I18N.message("addsyndication"));
		addSyndication.addClickHandler(event -> {
			list.deselectAllRecords();
			GUISyndication syndication = new GUISyndication();
			showSyndicationDetails(syndication);
		});

		toolStrip.addButton(refresh);
		toolStrip.addButton(addSyndication);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				SyndicationService.Instance.get().getSyndication(Long.parseLong(rec.getAttributeAsString("id")),
						new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUISyndication syndication) {
								showSyndicationDetails(syndication);
							}
						});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showsyndications", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	public void refresh() {
		list.refresh(new SyndicationsDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_FOLDER;
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
				SyndicationService.Instance.get().delete(id, new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showSyndicationDetails(null);
					}
				});
			}
		}));

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler(click -> {
			LD.contactingServer();
			SyndicationService.Instance.get().test(Long.parseLong(rec.getAttributeAsString("id")),
					new AsyncCallback<Boolean>() {
						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Boolean result) {
							LD.clearPrompt();
							if (result.booleanValue())
								SC.say(I18N.message("connectionestablished"));
							else
								SC.warn(I18N.message("connectionfailed"));
						}
					});
		});

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(click -> SyndicationService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), true, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "0");
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> SyndicationService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), false, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "2");
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem resetCache = new MenuItem();
		resetCache.setTitle(I18N.message("resetcache"));
		resetCache.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmresetcache"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						SyndicationService.Instance.get().resetCache(id, new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								GuiLog.info(I18N.message("cachedeleted"), null);
							}
						});
					}
				}));

		if ("0".equals(rec.getAttributeAsString(EENABLED)))
			contextMenu.setItems(test, disable, delete, resetCache);
		else
			contextMenu.setItems(test, enable, delete, resetCache);
		contextMenu.showContextMenu();
	}

	public void showSyndicationDetails(GUISyndication syndication) {
		if (!(details instanceof SyndicationDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new SyndicationDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((SyndicationDetailsPanel) details).setSyndication(syndication);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param syndication the syndication to update
	 */
	public void updateRecord(GUISyndication syndication) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, syndication.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", syndication.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", syndication.getName());
		rec.setAttribute("url", syndication.getUrl());
		rec.setAttribute(EENABLED, syndication.getEnabled() == 1 ? "0" : "2");
		rec.setAttribute("targetPath", syndication.getTargetPath());
		list.refreshRow(list.getRecordIndex(rec));
	}
}