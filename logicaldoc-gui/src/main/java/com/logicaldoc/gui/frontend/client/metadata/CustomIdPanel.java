package com.logicaldoc.gui.frontend.client.metadata;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIScheme;
import com.logicaldoc.gui.common.client.data.SequencesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SchemeService;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the set of filters associated to the current account
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class CustomIdPanel extends AdminPanel {

	private static final String VALUE = "value";

	private static final String TEMPLATE_ID = "templateId";

	private static final String EVALUATE_AT_UPDATE = "evaluateAtUpdate";

	private static final String EVALUATE_AT_CHECKIN = "evaluateAtCheckin";

	private static final String SCHEME = "scheme";

	private static final String TEMPLATE = "template";

	private RefreshableListGrid sequences;

	private List<GUIScheme> schemesData;

	public CustomIdPanel(List<GUIScheme> schemesData) {
		super("customid");
		this.schemesData = schemesData;
	}

	@Override
	public void onDraw() {
		if (Feature.enabled(Feature.CUSTOMID))
			body.setMembers(setupSchemesPanel(schemesData, GUIScheme.CUSTOMID_SCHEME));

		Tab autonamingTab = new Tab();
		autonamingTab.setTitle(I18N.message("autonaming"));
		autonamingTab.setPane(setupSchemesPanel(schemesData, GUIScheme.AUTONAMING_SCHEME));

		Tab autofoldingTab = new Tab();
		autofoldingTab.setTitle(I18N.message("autofolding"));
		autofoldingTab.setPane(setupSchemesPanel(schemesData, GUIScheme.AUTOFOLDING_SCHEME));

		Tab splittingTab = new Tab();
		splittingTab.setTitle(I18N.message("splitting"));
		splittingTab.setPane(setupSchemesPanel(schemesData, GUIScheme.SPLIT_SCHEME));

		Tab sequencesTab = new Tab();
		sequencesTab.setTitle(I18N.message("sequences"));
		sequencesTab.setPane(setupSequencesPanel());

		if (Feature.enabled(Feature.AUTO_NAMING))
			tabs.addTab(autonamingTab);
		if (Feature.enabled(Feature.AUTO_FOLDING))
			tabs.addTab(autofoldingTab);
		if (Feature.enabled(Feature.SPLIT))
			tabs.addTab(splittingTab);
		tabs.addTab(sequencesTab);
	}

	private VLayout setupSchemesPanel(List<GUIScheme> data, String type) {
		ListGridField template = new ListGridField("templateName", I18N.message(TEMPLATE));
		template.setWidth(120);
		template.setCanEdit(false);

		ListGridField scheme = new ListGridField(SCHEME, I18N.message(SCHEME));
		scheme.setWidth(200);
		scheme.setRequired(true);
		scheme.setEscapeHTML(true);

		final ListGridField evaluateAtCheckin = new ListGridField(EVALUATE_AT_CHECKIN,
				I18N.message("evaluateatcheckin"));
		evaluateAtCheckin.setWidth(150);
		evaluateAtCheckin.setType(ListGridFieldType.BOOLEAN);

		final ListGridField evaluateAtUpdate = new ListGridField(EVALUATE_AT_UPDATE, I18N.message("evaluateatupdate"));
		evaluateAtUpdate.setWidth(150);
		evaluateAtUpdate.setType(ListGridFieldType.BOOLEAN);

		final ListGrid customIds = new ListGrid();
		customIds.setEmptyMessage(I18N.message("notitemstoshow"));
		customIds.setShowAllRecords(true);
		customIds.setCanEdit(true);
		customIds.setWidth100();
		customIds.setHeight100();
		customIds.setFields(template);
		customIds.setSelectionType(SelectionStyle.SINGLE);
		customIds.setModalEditing(true);

		List<ListGridRecord> records = new ArrayList<>();
		if (data != null)
			for (GUIScheme cid : data) {
				if (!type.equals(cid.getType()))
					continue;
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute(TEMPLATE_ID, Long.toString(cid.getTemplateId()));
				rec.setAttribute("templateName", cid.getTemplateName());
				if (cid.getScheme() != null)
					rec.setAttribute(SCHEME, cid.getScheme());
				rec.setAttribute(EVALUATE_AT_CHECKIN, cid.isEvaluateAtCheckin());
				rec.setAttribute(EVALUATE_AT_UPDATE, cid.isEvaluateAtUpdate());
				rec.setAttribute("type", cid.getType());
				records.add(rec);
			}
		customIds.setData(records.toArray(new ListGridRecord[0]));

		if (GUIScheme.SPLIT_SCHEME.equals(type))
			customIds.setFields(template, scheme);
		else
			customIds.setFields(template, scheme, evaluateAtCheckin, evaluateAtUpdate);

		customIds.addCellContextClickHandler(event -> {
			showSchemeContextMenu(customIds);
			event.cancel();
		});

		customIds.addEditCompleteHandler(event -> {
			GUIScheme cid = new GUIScheme();
			ListGridRecord rec = customIds.getRecord(event.getRowNum());
			cid.setTemplateId(Long.parseLong(rec.getAttribute(TEMPLATE_ID)));
			cid.setEvaluateAtCheckin(rec.getAttributeAsBoolean(EVALUATE_AT_CHECKIN));
			cid.setEvaluateAtUpdate(rec.getAttributeAsBoolean(EVALUATE_AT_UPDATE));
			cid.setScheme(rec.getAttributeAsString(SCHEME));
			cid.setType(rec.getAttributeAsString("type"));

			SchemeService.Instance.get().save(cid, new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					// Nothing to do
				}
			});
		});

		VLayout sc = new VLayout();
		HTMLFlow hint = new HTMLFlow(I18N.message("customidhint"));
		hint.setMargin(3);
		sc.addMember(hint);
		sc.addMember(customIds);

		return sc;
	}

	private VLayout setupSequencesPanel() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.setAutoFit(true);
		refresh.addClickHandler(event -> sequences.refresh(new SequencesDS("customid-")));
		toolStrip.addButton(refresh);

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setWidth(60);
		id.setCanEdit(false);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(200);
		name.setCanEdit(false);
		name.setRequired(true);
		name.setCanFilter(true);
		name.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (value.toString().startsWith("customid-"))
				return value.toString().substring("customid-".length());
			else
				return value.toString();
		});

		final ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
		value.setWidth(80);
		value.setType(ListGridFieldType.INTEGER);
		value.setRequired(true);
		value.setCanFilter(true);

		sequences = new RefreshableListGrid(new SequencesDS("customid-"));
		sequences.setShowAllRecords(true);
		sequences.setCanEdit(true);
		sequences.setWidth100();
		sequences.setHeight100();
		sequences.setSelectionType(SelectionStyle.SINGLE);
		sequences.setModalEditing(true);
		sequences.setFilterOnKeypress(true);
		sequences.setShowFilterEditor(true);
		sequences.setFields(id, name, value);

		sequences.addEditCompleteHandler(event -> {
			ListGridRecord rec = sequences.getRecord(event.getRowNum());
			SchemeService.Instance.get().resetSequence(rec.getAttributeAsLong("id"), rec.getAttributeAsInt(VALUE),
					new AsyncCallback<>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void ret) {
							// Nothing to do
						}
					});
		});

		sequences.addCellContextClickHandler(ckick -> {
			showSequencesContextMenu();
			ckick.cancel();
		});

		VLayout sequencesPanel = new VLayout();
		sequencesPanel.setMembers(toolStrip, sequences);

		return sequencesPanel;
	}

	private void showSchemeContextMenu(final ListGrid schemes) {
		Menu contextMenu = new Menu();

		MenuItem clean = new MenuItem();
		clean.setTitle(I18N.message("clean"));
		clean.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmclean"), (Boolean confirm) -> {
					if (Boolean.TRUE.equals(confirm)) {
						final ListGridRecord rec = schemes.getSelectedRecord();
						SchemeService.Instance.get().delete(Long.parseLong(rec.getAttributeAsString(TEMPLATE_ID)),
								rec.getAttributeAsString("type"), new AsyncCallback<>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Void ret) {
										schemes.getSelectedRecord().setAttribute(SCHEME, (String) null);
										schemes.getSelectedRecord().setAttribute(EVALUATE_AT_CHECKIN, false);
										schemes.getSelectedRecord().setAttribute(EVALUATE_AT_UPDATE, false);
										schemes.refreshRow(schemes.getRecordIndex(rec));
									}
								});
					}
				}));

		contextMenu.setItems(clean);
		contextMenu.showContextMenu();
	}

	private void showSequencesContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = sequences.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean confirm) -> {
					if (Boolean.TRUE.equals(confirm)) {
						SchemeService.Instance.get().deleteSequence(id, new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								sequences.removeSelectedData();
								sequences.deselectAllRecords();
							}
						});
					}
				}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}