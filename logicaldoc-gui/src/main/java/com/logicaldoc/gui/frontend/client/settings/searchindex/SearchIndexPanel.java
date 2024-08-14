package com.logicaldoc.gui.frontend.client.settings.searchindex;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;
import com.logicaldoc.gui.common.client.data.IndexingQueueDS;
import com.logicaldoc.gui.common.client.data.LanguagesDS;
import com.logicaldoc.gui.common.client.data.ParsersDS;
import com.logicaldoc.gui.common.client.data.TokenFiltersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.validators.MinLengthValidator;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IconGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IndexedListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SearchEngineService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.PickerIconName;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the search and indexing infos.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SearchIndexPanel extends AdminPanel {

	private static final String SEPARATEDCOMMA = "separatedcomma";

	private static final String VALUE = "value";

	private static final String EENABLED = "eenabled";

	private static final String ALIASES = "aliases";

	private static final String EXTENSION = "extension";

	private Layout indexingQueueTabPanel;

	private GUISearchEngine searchEngine;

	private ValuesManager vm = new ValuesManager();

	private ListGrid parsersList;

	private RefreshableListGrid docsList;

	private ListGrid langsList;

	private ListGrid filtersGrid;

	public SearchIndexPanel() {
		super("searchandindexing");
	}

	@Override
	public void onDraw() {
		if (searchEngine == null)
			SearchEngineService.Instance.get().getInfo(new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUISearchEngine searchEngine) {
					SearchIndexPanel.this.searchEngine = searchEngine;
					fillSearchEngineTab();

					tabs.addTab(fillFiltersTab());
					tabs.addTab(fillLanguagesTab());
					tabs.addTab(fillParsersTab());
					tabs.addTab(fillHistoryTab());

					if (Session.get().isDefaultTenant()) {
						tabs.addTab(fillIndexingQueueTab(100));
						tabs.addTab(fillEntiesTab());
					}
				}
			});
	}

	protected Tab fillEntiesTab() {
		Tab entriesTab = new Tab(I18N.message("entries"));
		entriesTab.setPane(new SearchIndexEntriesPanel());
		return entriesTab;
	}

	private Tab fillIndexingQueueTab(final int maxValue) {
		Tab indexingQueueTab = new Tab(I18N.message("indexingqueue"));
		prepareIndexingQueuePanel(maxValue);
		indexingQueueTab.setPane(indexingQueueTabPanel);
		indexingQueueTab.addTabSelectedHandler(event -> refreshIndexingQueue(maxValue));
		return indexingQueueTab;
	}

	private Tab fillHistoryTab() {
		Tab historyTab = new Tab(I18N.message("history"));
		historyTab.setPane(new SearchIndexHistoryPanel());
		return historyTab;
	}

	private Tab fillParsersTab() {
		Tab parsersInfoTab = new Tab(I18N.message("parsers"));
		Layout parsersInfoTabPanel = new HLayout();
		parsersInfoTabPanel.setWidth100();
		parsersInfoTabPanel.setHeight100();

		setMembersMargin(3);

		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField icon = new IconGridField();
		icon.setHidden(false);

		ListGridField extension = new ListGridField(EXTENSION, I18N.message(EXTENSION), 80);
		extension.setCanEdit(false);
		extension.setValidators(new MinLengthValidator(1));

		ListGridField name = new ListGridField("name", I18N.message("name"), 180);
		name.setCanEdit(false);
		name.setValidators(new MinLengthValidator(1));

		ListGridField aliases = new ListGridField(ALIASES, I18N.message(ALIASES));
		aliases.setCanEdit(true);
		aliases.setCanFilter(false);
		aliases.setCanSort(false);
		aliases.setEscapeHTML(true);

		parsersList = new ListGrid();
		parsersList.setCanEdit(true);
		parsersList.setSelectionType(SelectionStyle.SINGLE);
		parsersList.setWidth100();
		parsersList.setHeight100();
		parsersList.setAutoFetchData(true);
		parsersList.setFields(icon, extension, name, aliases);
		parsersList.setDataSource(ParsersDS.get());
		parsersList.setShowFilterEditor(true);
		parsersList.setFilterOnKeypress(true);
		parsersList.setModalEditing(true);

		parsersList.addEditCompleteHandler(event -> {
			ListGridRecord rec = parsersList.getRecord(event.getRowNum());

			SearchEngineService.Instance.get().setAliases(rec.getAttributeAsString(EXTENSION),
					(String) event.getNewValues().get(ALIASES), new AsyncCallback<>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void ret) {
							parsersList.invalidateCache();
							parsersList.getDataSource().invalidateCache();
							parsersList.redraw();
						}
					});
		});

		parsersInfoTabPanel.addMember(parsersList);
		parsersInfoTab.setPane(parsersInfoTabPanel);
		return parsersInfoTab;
	}

	private Tab fillLanguagesTab() {
		Tab languagesTab = new Tab(I18N.message("languages"));
		Layout languagesTabPanel = new HLayout();
		languagesTabPanel.setWidth100();
		languagesTabPanel.setHeight100();

		ListGridField enabled = new ListGridField(EENABLED, " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		ListGridField code = new ListGridField("code", I18N.message("code"), 80);
		code.setCanEdit(false);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanEdit(false);

		langsList = new ListGrid();
		langsList.setCanEdit(false);
		langsList.setWidth100();
		langsList.setHeight100();
		langsList.setAutoFetchData(true);
		langsList.setDataSource(new LanguagesDS(false));
		langsList.setShowFilterEditor(true);
		langsList.setFilterOnKeypress(true);
		langsList.setSelectionType(SelectionStyle.SINGLE);
		langsList.setFields(enabled, code, name);

		languagesTabPanel.addMember(langsList);
		languagesTab.setPane(languagesTabPanel);

		if (Feature.enabled(Feature.INDEX_LANGUAGES))
			langsList.addCellContextClickHandler(event -> {
				showLanguagesMenu();
				event.cancel();
			});

		return languagesTab;
	}

	private Tab fillFiltersTab() {
		ListGridField enabled = new ListGridField(EENABLED, " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		ListGridField name = new ListGridField("name", I18N.message("filter"));
		name.setWidth("*");
		name.setCanEdit(false);

		filtersGrid = new ListGrid() {
			@Override
			protected Canvas getExpansionComponent(final ListGridRecord rec) {
				return buildFiltersGridExpansionComponent(rec);
			}
		};

		filtersGrid.setShowRowNumbers(true);
		filtersGrid.setCanExpandRecords(true);
		filtersGrid.setCanEdit(false);
		filtersGrid.setWidth100();
		filtersGrid.setHeight100();
		filtersGrid.setAutoFetchData(true);
		filtersGrid.setSelectionType(SelectionStyle.SINGLE);
		filtersGrid.setCanReorderRecords(true);
		filtersGrid.setDataSource(new TokenFiltersDS(null));
		filtersGrid.setFields(enabled, name);

		Layout filtersTabPanel = new HLayout();
		filtersTabPanel.setWidth100();
		filtersTabPanel.setHeight100();
		filtersTabPanel.addMember(filtersGrid);

		Tab filtersTab = new Tab(I18N.message("filters"));
		filtersTab.setPane(filtersTabPanel);

		filtersGrid.addDropCompleteHandler(event -> {
			List<String> filters = new ArrayList<>();
			ListGridRecord[] records = filtersGrid.getRecords();
			for (ListGridRecord rec : records)
				filters.add(rec.getAttributeAsString("name"));

			SearchEngineService.Instance.get().reorderTokenFilters(filters, new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg) {
					// Nothing to do
				}
			});
		});

		filtersGrid.addCellContextClickHandler(event -> {
			showFilterMenu(filtersGrid);
			event.cancel();
		});
		return filtersTab;
	}

	private Canvas buildFiltersGridExpansionComponent(ListGridRecord rec) {
		String filter = rec.getAttributeAsString("name");

		VLayout layout = new VLayout(5);
		layout.setPadding(5);

		final ListGrid configsGrid = new ListGrid();
		configsGrid.setHeight(150);
		configsGrid.setDataSource(new TokenFiltersDS(filter));
		configsGrid.setCanEdit(true);
		configsGrid.setModalEditing(true);
		configsGrid.setAutoSaveEdits(true);
		configsGrid.setAutoFetchData(true);

		ListGridField name = new ListGridField("name", I18N.message("parameter"), 150);
		name.setCanEdit(false);
		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
		value.setWidth("*");
		value.setCanEdit(true);
		configsGrid.setFields(name, value);

		layout.addMember(configsGrid);

		HLayout hLayout = new HLayout(10);
		hLayout.setAlign(Alignment.CENTER);

		IButton saveButton = new IButton(I18N.message("save"));
		saveButton.setTop(250);
		saveButton.addClickHandler(event -> {
			final List<GUIParameter> params = new ArrayList<>();
			ListGridRecord[] records = configsGrid.getRecords();
			for (ListGridRecord recd : records) {
				params.add(new GUIParameter(recd.getAttributeAsString("name"), recd.getAttributeAsString(VALUE)));
			}

			SearchEngineService.Instance.get().saveTokenFilterSettings(filter, params, new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg) {
					// Nothing to do
				}
			});
		});
		hLayout.addMember(saveButton);

		IButton closeButton = new IButton(I18N.message("close"));
		closeButton.addClickHandler(event -> filtersGrid.collapseRecord(rec));
		hLayout.addMember(closeButton);

		layout.addMember(hLayout);

		return layout;
	}

	private void fillSearchEngineTab() {
		Layout searchEngineTabPanel = new VLayout();
		searchEngineTabPanel.setWidth100();
		searchEngineTabPanel.setHeight100();

		DynamicForm searchEngineForm = new DynamicForm();
		searchEngineForm.setTitleOrientation(TitleOrientation.LEFT);
		searchEngineForm.setNumCols(2);
		searchEngineForm.setWrapItemTitles(false);
		searchEngineForm.setColWidths(1, "*");
		searchEngineForm.setValuesManager(vm);

		PickerIcon computeStat = new PickerIcon(PickerIconName.REFRESH, iconClick -> {
			iconClick.getItem().setValue(I18N.message("computing") + "...");
			SearchEngineService.Instance.get().countEntries(new AsyncCallback<Long>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Long count) {
					iconClick.getItem().setValue(Util.formatLong(count));
				}
			});
		});

		computeStat.setPrompt(I18N.message("calculatestats"));
		computeStat.setWidth(16);

		// Entries count
		StaticTextItem entries = ItemFactory.newStaticTextItem("entries", "entriescount", "-");
		entries.setIconHSpace(2);
		entries.setIconWidth(16);
		entries.setIconHeight(16);
		entries.setIcons(computeStat);
		entries.setWidth("1%");

		// Locked
		StaticTextItem status = ItemFactory.newStaticTextItem("status",
				this.searchEngine.isLocked() ? ("<span style='color:red'>" + I18N.message("locked") + "</span>")
						: I18N.message("unlocked"));
		status.setRedrawOnChange(true);

		// Include Patters
		TextItem includePatterns = ItemFactory.newTextItem("includepatterns", null);
		includePatterns.setValue(this.searchEngine.getIncludePatters());
		includePatterns.setHint(I18N.message(SEPARATEDCOMMA));
		includePatterns.setHintStyle("hint");
		includePatterns.setWidth(300);

		// Exclude Patters
		TextItem excludePatterns = ItemFactory.newTextItem("excludepatterns", null);
		excludePatterns.setValue(this.searchEngine.getExcludePatters());
		excludePatterns.setHint(I18N.message(SEPARATEDCOMMA));
		excludePatterns.setHintStyle("hint");
		excludePatterns.setWidth(300);

		// Include Patters
		TextItem includePatternsMetadata = ItemFactory.newTextItem("includepatternsmetadata", null);
		includePatternsMetadata.setValue(this.searchEngine.getIncludePattersMetadata());
		includePatternsMetadata.setHint(I18N.message(SEPARATEDCOMMA));
		includePatternsMetadata.setHintStyle("hint");
		includePatternsMetadata.setWidth(300);

		// Exclude Patters
		TextItem excludePatternsMetadata = ItemFactory.newTextItem("excludepatternsmetadata", null);
		excludePatternsMetadata.setValue(this.searchEngine.getExcludePatternsMetadata());
		excludePatternsMetadata.setHint(I18N.message(SEPARATEDCOMMA));
		excludePatternsMetadata.setHintStyle("hint");
		excludePatternsMetadata.setWidth(300);

		SelectItem sorting = ItemFactory.newSelectItem("sorting");
		LinkedHashMap<String, String> opts = new LinkedHashMap<>();
		opts.put("", I18N.message("none").toLowerCase());
		opts.put("oldestfirst", I18N.message("oldestfirst"));
		opts.put("mostrecentfirst", I18N.message("mostrecentfirst"));
		opts.put("smallestfirst", I18N.message("smallestfirst"));
		opts.put("biggestfirst", I18N.message("biggestfirst"));
		sorting.setValueMap(opts);
		sorting.setValue(this.searchEngine.getSorting());
		sorting.setDisabled(
				this.searchEngine.getCustomSorting() != null && !this.searchEngine.getCustomSorting().isEmpty());
		sorting.setVisible(Session.get().isDefaultTenant());

		TextItem customSorting = ItemFactory.newTextItem("customsorting", this.searchEngine.getCustomSorting());
		customSorting.setWidth(300);
		customSorting.addChangeHandler(changeEvent -> sorting
				.setDisabled(changeEvent.getValue() != null && !changeEvent.getValue().toString().isEmpty()));

		// The optional batch
		SpinnerItem batch = ItemFactory.newSpinnerItem("batch", this.searchEngine.getBatch());
		batch.setMin(1);
		batch.setStep(50);
		batch.setWidth(100);
		batch.setVisible(Session.get().isDefaultTenant());

		// The number of threads
		SpinnerItem threads = ItemFactory.newSpinnerItem("threads", this.searchEngine.getThreads());
		threads.setRequired(true);
		threads.setMin(1);
		threads.setStep(1);
		threads.setVisible(Session.get().isDefaultTenant());

		// The optional parse timeout
		SpinnerItem timeout = ItemFactory.newSpinnerItem("timeout", "parsingtimeout",
				this.searchEngine.getParsingTimeout());
		timeout.setHint(I18N.message("seconds").toLowerCase());
		timeout.setWidth(100);
		timeout.setMin(0);
		timeout.setStep(10);

		// Retain extracted text on timeout
		RadioGroupItem timeoutRetain = ItemFactory.newBooleanSelector("timeoutRetain", "ontimeoutretaintext");
		timeoutRetain.setHint(I18N.message("ontimeoutretaintexthint"));
		timeoutRetain.setValue(this.searchEngine.isParsingTimeoutRetain() ? "yes" : "no");
		timeoutRetain.setRequired(true);
		
		// The optional max text that will be put in the index
		SpinnerItem maxText = ItemFactory.newSpinnerItem("maxtext", this.searchEngine.getMaxText());
		maxText.setHint(I18N.message("maxtextinindex"));
		maxText.setWidth(100);
		maxText.setMin(0);
		maxText.setStep(100);
		maxText.setVisible(Session.get().isDefaultTenant());

		// The optional max size elaborated when parsing text files
		SpinnerItem maxTextFileSize = ItemFactory.newSpinnerItem("maxtextfilesize",
				this.searchEngine.getMaxTextFileSize());
		maxTextFileSize.setHint(I18N.message("maxtextfilesizehint"));
		maxTextFileSize.setWidth(100);
		maxTextFileSize.setMin(0);
		maxTextFileSize.setStep(1048);

		// Repository
		TextItem repository = ItemFactory.newTextItem("repository", null);
		repository.setValue(this.searchEngine.getDir());
		repository.setWidth(300);
		repository.setVisible(Session.get().isDefaultTenant());

		// Mark unindexable on error
		RadioGroupItem skipOnError = ItemFactory.newBooleanSelector("skipOnError", "onerrormarkunindexable");
		skipOnError.setHint(I18N.message("onerrormarkunindexablehint"));
		skipOnError.setValue(this.searchEngine.isSkipOnError() ? "yes" : "no");
		skipOnError.setRequired(true);

		HLayout buttons = prepareButtons();

		searchEngineForm.setItems(entries, status, repository, includePatterns, excludePatterns,
				includePatternsMetadata, excludePatternsMetadata, skipOnError, sorting, customSorting, threads, batch,
				timeout, timeoutRetain, maxText, maxTextFileSize);

		buttons.setMembersMargin(5);
		searchEngineTabPanel.setMembers(searchEngineForm, buttons);
		searchEngineTabPanel.setMembersMargin(15);
		searchEngineTabPanel.setMargin(5);

		body.setMembers(searchEngineTabPanel);
	}

	private HLayout prepareButtons() {
		HLayout buttons = new HLayout();

		IButton save = prepareSaveButton();

		IButton unlock = prepareUnlockButton();

		IButton rescheduleAll = prepareRescheduleAllButton();

		IButton dropIndex = prepareDropIndexButton(rescheduleAll);

		IButton check = prepareCheckButton();

		IButton purge = preparePurgeButton();

		if (Session.get().isDefaultTenant()) {
			buttons.setMembers(save, unlock, purge, rescheduleAll, dropIndex, check);
		} else {
			buttons.setMembers(save, rescheduleAll);
		}
		return buttons;
	}

	private IButton preparePurgeButton() {
		IButton purge = new IButton(I18N.message("purge"));
		purge.setAutoFit(true);
		purge.addClickHandler(purgeClick -> SC.ask(I18N.message("purgeconfirmation"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				LD.contactingServer();
				SearchEngineService.Instance.get().purge(new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						LD.clearPrompt();
					}
				});
			}
		}));
		return purge;
	}

	private IButton prepareCheckButton() {
		IButton check = new IButton(I18N.message("check"));
		check.setAutoFit(true);
		check.addClickHandler(checkClick -> {
			LD.contactingServer();
			SearchEngineService.Instance.get().check(new AsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
					LD.clearPrompt();
				}

				@Override
				public void onSuccess(String ret) {
					LD.clearPrompt();
					SearchIndexCheckStatus sc = new SearchIndexCheckStatus(ret);
					sc.show();
				}
			});
		});
		return check;
	}

	private IButton prepareDropIndexButton(IButton rescheduleAll) {
		IButton dropIndex = new IButton(I18N.message("dropindex"));
		dropIndex.setAutoFit(true);
		dropIndex.addClickHandler((ClickEvent dropIndexClick) -> LD.ask(I18N.message("question"),
				I18N.message("confirmdropindex"), yes -> {
					if (Boolean.TRUE.equals(yes)) {
						LD.contactingServer();
						rescheduleAll.setDisabled(true);
						SearchEngineService.Instance.get().rescheduleAll(true, new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								LD.clearPrompt();
								dropIndex.setDisabled(false);
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								GuiLog.info(I18N.message("docsreindex"), null);
								dropIndex.setDisabled(false);
								LD.clearPrompt();
								AdminScreen.get().setContent(new SearchIndexPanel());
							}
						});
					}
				}));
		return dropIndex;
	}

	private IButton prepareRescheduleAllButton() {
		final IButton rescheduleAll = new IButton(I18N.message("rescheduleall"));
		rescheduleAll.setAutoFit(true);
		rescheduleAll.addClickHandler(rescheduleAllClick -> LD.ask(I18N.message("question"),
				I18N.message("confirmreindex"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						LD.contactingServer();
						rescheduleAll.setDisabled(true);
						SearchEngineService.Instance.get().rescheduleAll(false, new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								LD.clearPrompt();
								rescheduleAll.setDisabled(false);
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								GuiLog.info(I18N.message("docsreindex"), null);
								rescheduleAll.setDisabled(false);
								LD.clearPrompt();
								AdminScreen.get().setContent(new SearchIndexPanel());
							}
						});
					}
				}));
		return rescheduleAll;
	}

	private IButton prepareUnlockButton() {
		IButton unlock = new IButton(I18N.message("unlock"));
		unlock.setAutoFit(true);
		unlock.addClickHandler(unlockClick -> SearchEngineService.Instance.get().unlock(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void ret) {
				GuiLog.info(I18N.message("indexunlocked"), null);
				AdminScreen.get().setContent(new SearchIndexPanel());
			}
		}));
		return unlock;
	}

	private IButton prepareSaveButton() {
		IButton save = new IButton(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(saveClick -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			collectValues();

			SearchEngineService.Instance.get().save(SearchIndexPanel.this.searchEngine, new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					AdminScreen.get().setContent(new SearchIndexPanel());
				}
			});
		});
		return save;
	}

	private void collectValues() {
		@SuppressWarnings("unchecked")
		final Map<String, Object> values = vm.getValues();

		SearchIndexPanel.this.searchEngine.setIncludePatterns((String) values.get("includepatterns"));
		SearchIndexPanel.this.searchEngine.setExcludePatterns((String) values.get("excludepatterns"));
		SearchIndexPanel.this.searchEngine.setIncludePatternsMetadata((String) values.get("includepatternsmetadata"));
		SearchIndexPanel.this.searchEngine.setExcludePatternsMetadata((String) values.get("excludepatternsmetadata"));
		SearchIndexPanel.this.searchEngine.setDir((String) values.get("repository"));
		SearchIndexPanel.this.searchEngine.setSorting((String) values.get("sorting"));
		SearchIndexPanel.this.searchEngine.setCustomSorting((String) values.get("customsorting"));
		SearchIndexPanel.this.searchEngine.setSkipOnError("yes".equals(values.get("skipOnError")));
		SearchIndexPanel.this.searchEngine.setParsingTimeoutRetain("yes".equals(values.get("timeoutRetain")));

		String btch = vm.getValueAsString("batch");
		if (btch == null || "".equals(btch.trim()))
			SearchIndexPanel.this.searchEngine.setBatch(0);
		else
			SearchIndexPanel.this.searchEngine.setBatch(Integer.parseInt(btch));

		String thrds = vm.getValueAsString("threads");
		if (thrds == null || "".equals(thrds.trim()))
			SearchIndexPanel.this.searchEngine.setThreads(1);
		else
			SearchIndexPanel.this.searchEngine.setThreads(Integer.parseInt(thrds));

		String timeoutValue = vm.getValueAsString("timeout");
		if (timeoutValue == null || "".equals(timeoutValue.trim()))
			SearchIndexPanel.this.searchEngine.setParsingTimeout(0);
		else
			SearchIndexPanel.this.searchEngine.setParsingTimeout(Integer.parseInt(timeoutValue));

		String maxtext = vm.getValueAsString("maxtext");
		if (maxtext == null || "".equals(maxtext.trim()))
			SearchIndexPanel.this.searchEngine.setMaxText(0);
		else
			SearchIndexPanel.this.searchEngine.setMaxText(Integer.parseInt(maxtext));

		String maxTextFileSizeValue = vm.getValueAsString("maxtextfilesize");
		if (maxtext == null || "".equals(maxtext.trim()))
			SearchIndexPanel.this.searchEngine.setMaxTextFileSize(0);
		else
			SearchIndexPanel.this.searchEngine.setMaxTextFileSize(Integer.parseInt(maxTextFileSizeValue));
	}

	private void prepareIndexingQueuePanel(Integer maxValue) {
		final SpinnerItem max = ItemFactory.newSpinnerItem("max", "", maxValue, 5, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setStep(10);
		max.addChangedHandler(event -> refreshIndexingQueue((Integer) max.getValue()));

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		display.addClickHandler(event -> {
			if (Boolean.TRUE.equals(max.validate()))
				refreshIndexingQueue((Integer) max.getValue());
		});

		// Prepare a panel containing a title and the documents number
		final InfoPanel infoPanel = new InfoPanel("");

		ListGridField id = new ColoredListGridField("id");
		id.setWidth(50);
		id.setHidden(true);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.FLOAT);

		ListGridField version = new VersionListGridField();
		version.setAlign(Alignment.CENTER);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");

		ListGridField publisher = new ColoredListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);

		ListGridField published = new DateListGridField("published", "publishedon");

		ListGridField creator = new ColoredListGridField("creator", I18N.message("creator"), 90);
		creator.setAlign(Alignment.CENTER);

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);

		ListGridField indexable = new IndexedListGridField();

		ListGridField filename = new FileNameListGridField();

		ListGridField lockUserId = new ColoredListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);

		docsList = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if (rec == null)
					return "";
				if (getFieldName(colNum).equals("filename")) {
					if ("stop".equals(rec.getAttribute("immutable"))) {
						return "color: #888888; font-style: italic;";
					} else {
						return super.getCellCSSText(rec, rowNum, colNum);
					}
				} else {
					return super.getCellCSSText(rec, rowNum, colNum);
				}
			}
		};
		docsList.setEmptyMessage(I18N.message("notitemstoshow"));

		docsList.addCellContextClickHandler(event -> {
			showIndexQueueMenu();
			event.cancel();
		});

		docsList.addDataArrivedHandler(dataArrivedEvent -> infoPanel
				.setMessage(I18N.message("showndocuments", Integer.toString(docsList.getTotalRows()))));

		docsList.setShowRecordComponents(true);
		docsList.setShowRecordComponentsByCell(true);
		docsList.setCanFreezeFields(true);
		docsList.setAutoFetchData(true);
		docsList.setSelectionType(SelectionStyle.MULTIPLE);
		docsList.setShowFilterEditor(true);
		docsList.setFilterOnKeypress(true);
		docsList.setFields(id, indexable, filename, size, lastModified, version, publisher, published, creator, created,
				customId);

		indexingQueueTabPanel = new VLayout();
		indexingQueueTabPanel.setMembers(toolStrip, infoPanel, docsList);
	}

	private void refreshIndexingQueue(Integer max) {
		docsList.refresh(new IndexingQueueDS(max));
	}

	private void showIndexQueueMenu() {
		final ListGridRecord[] selection = docsList.getSelectedRecords();

		Menu contextMenu = new Menu();
		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> {
			ListGridRecord rec = docsList.getSelectedRecord();
			if (rec == null)
				return;

			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong("id"));
		});

		MenuItem markUnindexable = new MenuItem();
		markUnindexable.setTitle(I18N.message("markunindexable"));
		markUnindexable.addClickHandler(event -> {
			if (selection == null)
				return;
			List<Long> ids = new ArrayList<>();
			for (int j = 0; j < selection.length; j++)
				ids.add(selection[j].getAttributeAsLong("id"));

			DocumentService.Instance.get().markUnindexable(ids, new AsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					for (ListGridRecord rec : selection) {
						docsList.removeData(rec);
					}
				}
			});
		});

		contextMenu.setItems(markUnindexable, openInFolder);
		contextMenu.showContextMenu();
	}

	private void showLanguagesMenu() {
		final ListGridRecord rec = langsList.getSelectedRecord();

		Menu contextMenu = new Menu();
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> SearchEngineService.Instance.get()
				.setLanguageStatus(rec.getAttributeAsString("code"), true, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "0");
						langsList.refreshRow(langsList.getRecordIndex(rec));
						GuiLog.info(I18N.message("settingsaffectnewsessions"), null);
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> SearchEngineService.Instance.get()
				.setLanguageStatus(rec.getAttributeAsString("code"), false, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "2");
						langsList.refreshRow(langsList.getRecordIndex(rec));
						GuiLog.info(I18N.message("settingsaffectnewsessions"), null);
					}
				}));

		if ("0".equals(rec.getAttributeAsString(EENABLED)))
			contextMenu.setItems(disable);
		else
			contextMenu.setItems(enable);
		contextMenu.showContextMenu();
	}

	private void showFilterMenu(final ListGrid grid) {
		final ListGridRecord rec = grid.getSelectedRecord();

		Menu contextMenu = new Menu();
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> SearchEngineService.Instance.get()
				.setTokenFilterStatus(rec.getAttributeAsString("name"), true, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "0");
						grid.refreshRow(grid.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> SearchEngineService.Instance.get()
				.setTokenFilterStatus(rec.getAttributeAsString("name"), false, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "2");
						grid.refreshRow(grid.getRecordIndex(rec));
					}
				}));

		if ("0".equals(rec.getAttributeAsString(EENABLED)))
			contextMenu.setItems(disable);
		else
			contextMenu.setItems(enable);
		contextMenu.showContextMenu();
	}
}