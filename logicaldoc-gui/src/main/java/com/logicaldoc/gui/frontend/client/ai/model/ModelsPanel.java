package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.RunningListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.logicaldoc.gui.frontend.client.ai.embedding.EmbeddingSettings;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
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
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of models
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelsPanel extends VLayout {

	private static final String QUESTION = "question";

	private static final String TRAINED = "trained";

	private static final String EVALUATED = "evaluated";

	private static final String EVALUATING = "evaluating";

	private static final String ID = "id";

	private static final String TRAINING = "training";

	static final Canvas SELECT_MODEL = new HTMLPanel("&nbsp;" + I18N.message("selectamodel"));

	private static final String LABEL = "label";

	private static final String DESCRIPTION = "description";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_MODEL;

	private Timer timer;

	public ModelsPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		InfoPanel infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_MODEL;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanFilter(true);
		name.setCanSort(true);
		name.setMinWidth(110);
		name.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL), 200);
		label.setCanFilter(true);
		label.setCanSort(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 300);
		description.setCanFilter(true);
		description.setCanSort(false);

		ListGridField modelType = new ListGridField("type", I18N.message("type"));
		modelType.setAutoFit(AutoFitWidthApproach.BOTH);
		modelType.setCellFormatter((value, rcrd, rowNum, colNum) -> I18N.message("aimodeltype." + value));

		ListGridField training = new RunningListGridField(TRAINING);
		training.setTitle(I18N.message(TRAINING));
		training.setAutoFitWidth(true);
		training.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		DateListGridField trained = new DateListGridField(TRAINED, I18N.message("lasttrained"));

		ListGridField evaluating = new RunningListGridField(EVALUATING);
		evaluating.setTitle(I18N.message(EVALUATING));
		evaluating.setAutoFitWidth(true);
		evaluating.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		DateListGridField evaluated = new DateListGridField(EVALUATED, I18N.message("lastevaluated"));

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, name, label, modelType, training, trained, evaluating, evaluated, description);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new ModelsDS());

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(click -> refresh());
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addmodel"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> onAddModel());

		toolStrip.addSeparator();

		ToolStripButton settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		toolStrip.addButton(settings);
		settings.addClickHandler(event -> new EmbeddingSettings().show());

		ToolStripButton stats = new ToolStripButton();
		stats.setTitle(I18N.message("stats"));
		toolStrip.addButton(stats);
		stats.addClickHandler(event -> new AIStats().show());

		toolStrip.addSeparator();

		ToolStripButton importModel = new ToolStripButton();
		importModel.setTitle(I18N.message("import"));
		toolStrip.addButton(importModel);
		importModel.addClickHandler(event -> new ModelImporter(changed -> refresh()).show());

		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				AIService.Instance.get().getModel(rec.getAttributeAsLong(ID), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIModel model) {
						showModelDetails(model);
					}
				});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showmodels", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);

		/*
		 * Create the timer that synchronize the view
		 */
		timer = new Timer() {
			public void run() {
				loadModels();
			}
		};
		timer.scheduleRepeating(5 * 1000);
	}

	private void refresh() {
		list.refresh(new ModelsDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_MODEL;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(rec.getAttributeAsLong(ID));

		String type = selection[0].getAttributeAsString("type");

		Long selectedModelId = selection[0].getAttributeAsLong("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				AIService.Instance.get().deleteModels(ids, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showModelDetails(null);
					}
				});
			}
		}));

		MenuItem train = new MenuItem();
		train.setTitle(I18N.message("starttraining"));
		train.addClickHandler(event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmtraining"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				LD.contactingServer();
				AIService.Instance.get().trainModel(ids.get(0), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						// empty
					}
				});
			}
		}));
		train.setEnabled(!selection[0].getAttributeAsBoolean(TRAINING));

		MenuItem stopTraining = new MenuItem();
		stopTraining.setTitle(I18N.message("stoptraining"));
		stopTraining.addClickHandler(
				event -> AIService.Instance.get().stopTraining(ids.get(0), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						// Nothing to do
					}
				}));
		stopTraining.setEnabled(selection[0].getAttributeAsBoolean(TRAINING));

		MenuItem evaluate = new MenuItem();
		evaluate.setTitle(I18N.message("startevaluation"));
		evaluate.addClickHandler(event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmevaluation"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				LD.contactingServer();
				AIService.Instance.get().evaluateModel(ids.get(0), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						// empty
					}
				});
			}
		}));
		evaluate.setEnabled(
				!selection[0].getAttributeAsBoolean(TRAINING) && !selection[0].getAttributeAsBoolean(EVALUATING)
						&& "neural".equals(selection[0].getAttributeAsString("type"))
						&& selection[0].getAttribute(TRAINED) != null);

		MenuItem query = new MenuItem();
		query.setTitle(I18N.message("querymodel"));
		query.addClickHandler(event -> AIService.Instance.get().getModel(selectedModelId, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUIModel mdl) {
				new QueryDialog(mdl).show();
			}
		}));
		query.setEnabled(!selection[0].getAttributeAsBoolean(TRAINING)
				&& !selection[0].getAttributeAsBoolean(EVALUATING) && selection[0].getAttribute(TRAINED) != null);

		MenuItem clone = new MenuItem();
		clone.setTitle(I18N.message("clone"));
		clone.addClickHandler(click -> LD.askForString("clone", "name",
				selection[0].getAttributeAsString("name") + "Cloned",
				value -> AIService.Instance.get().cloneModel(selectedModelId, value, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIModel mdl) {
						refresh();
					}
				})));

		query.setEnabled(!selection[0].getAttributeAsBoolean(TRAINING)
				&& !selection[0].getAttributeAsBoolean(EVALUATING) && selection[0].getAttribute(TRAINED) != null);

		MenuItem export = new MenuItem();
		export.setTitle(I18N.message("export"));
		export.addClickHandler(click -> new ModelExporter(selection[0].getAttributeAsLong("id")).show());

		MenuItem iimport = new MenuItem();
		iimport.setTitle(I18N.message("import"));
		iimport.addClickHandler(
				click -> new ModelImporter(selection[0].getAttributeAsString("name"), changed -> refresh()).show());

		if (!"zeroshot".equals(type)) {
			contextMenu.setItems(query, new MenuItemSeparator(), train, stopTraining, evaluate, new MenuItemSeparator(),
					clone, iimport, export, new MenuItemSeparator(), delete);
		} else {
			contextMenu.setItems(query, new MenuItemSeparator(), clone, iimport, export, new MenuItemSeparator(),
					delete);
		}

		contextMenu.showContextMenu();
	}

	protected void showModelDetails(GUIModel model) {
		detailsContainer.removeMember(details);
		if (model != null)
			details = new ModelDetailsPanel(this);
		else
			details = SELECT_MODEL;
		detailsContainer.addMember(details);
		((ModelDetailsPanel) details).setModel(model);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param model the model to take data from
	 */
	public void updateRecord(GUIModel model) {
		Record rec = list.find(new AdvancedCriteria(ID, OperatorId.EQUALS, model.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute(ID, model.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("type", model.getType());
		rec.setAttribute("name", model.getName());
		rec.setAttribute(LABEL,
				model.getLabel() != null && !model.getLabel().trim().isEmpty() ? model.getLabel() : model.getName());
		rec.setAttribute(DESCRIPTION, model.getDescription());
		rec.setAttribute(TRAINING, model.getTraining().isTraining());
		if (model.isNeuralNetwork())
			rec.setAttribute(EVALUATING, model.getEvaluation().isEvaluating());
		list.refreshRow(list.getRecordIndex(rec));

		if (somethigChangedInStatus(model, rec) || somethigChangedInLastProcessed(model, rec))
			ModelController.get().changed(model);
	}

	private boolean somethigChangedInLastProcessed(GUIModel model, Record rec) {
		try {
			Date recLastEvaluated = rec.getAttributeAsDate(EVALUATED);
			Date recLastTrained = rec.getAttributeAsDate(TRAINED);

			boolean changesInEvaluation = (recLastEvaluated == null && model.getEvaluation().getLastEvaluated() != null)
					|| (recLastEvaluated != null && !recLastEvaluated.equals(model.getEvaluation().getLastEvaluated()));
			boolean changesInTraining = (recLastTrained == null && model.getTraining().getLastTrained() != null)
					|| (recLastTrained != null && !recLastTrained.equals(model.getTraining().getLastTrained()));

			return changesInEvaluation || changesInTraining;
		} catch (Exception e) {
			GuiLog.error(model.getName() + " " + e.getMessage());
			return false;
		}
	}

	private boolean somethigChangedInStatus(GUIModel model, Record rec) {
		return !rec.getAttributeAsBoolean(TRAINING).equals(model.getTraining().isTraining())
				|| !rec.getAttributeAsBoolean(EVALUATING).equals(model.getEvaluation().isEvaluating());
	}

	protected void onAddModel() {
		list.deselectAllRecords();
		showModelDetails(new GUIModel());
	}

	private void loadModels() {
		AIService.Instance.get().getModels(new AsyncCallback<List<GUIModel>>() {
			@Override
			public void onFailure(Throwable caught) {
				// empty
			}

			@Override
			public void onSuccess(List<GUIModel> models) {
				for (GUIModel guiModel : models) {
					updateRecord(guiModel);
				}
			}
		});
	}

	@Override
	public void destroy() {
		super.destroy();
		this.timer.cancel();
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