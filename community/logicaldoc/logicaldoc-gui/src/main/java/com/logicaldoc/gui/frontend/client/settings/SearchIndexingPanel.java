package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.data.LanguagesDS;
import com.logicaldoc.gui.common.client.data.ParsersDS;
import com.logicaldoc.gui.common.client.data.TokenFiltersDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SearchEngineService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DropCompleteEvent;
import com.smartgwt.client.widgets.events.DropCompleteHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon.Picker;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the search and indexing infos.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class SearchIndexingPanel extends AdminPanel {

	private Layout searchEngineTabPanel;

	private Layout parsersInfoTabPanel;

	private Layout indexingQueueTabPanel;

	private GUISearchEngine searchEngine;

	private ValuesManager vm = new ValuesManager();

	private ListGrid parsersList;

	private ListGrid docsList;

	private ListGrid langsList;

	private DocumentsDS dataSource;

	private InfoPanel infoPanel;

	private Tab indexingQueueTab;

	public SearchIndexingPanel(GUISearchEngine searchEngine) {
		super("searchandindexing");

		this.searchEngine = searchEngine;

		fillSearchEngineTab(searchEngine);

		Tab parsersInfoTab = fillParsersTab();

		indexingQueueTab = fillIndexingQueueTab(100);

		tabs.addTab(fillFiltersTab());
		tabs.addTab(fillLanguagesTab());
		tabs.addTab(parsersInfoTab);
		tabs.addTab(indexingQueueTab);
	}

	private Tab fillIndexingQueueTab(int maxValue) {
		Tab indexingQueueTab = new Tab(I18N.message("indexingqueue"));
		refreshIndexingQueue(maxValue);
		indexingQueueTab.setPane(indexingQueueTabPanel);
		return indexingQueueTab;
	}

	private Tab fillParsersTab() {
		Tab parsersInfoTab = new Tab(I18N.message("parsers"));
		parsersInfoTabPanel = new HLayout();
		parsersInfoTabPanel.setWidth100();
		parsersInfoTabPanel.setHeight100();

		setMembersMargin(3);

		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		LengthRangeValidator validator = new LengthRangeValidator();
		validator.setMin(1);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanEdit(false);
		icon.setCanFilter(false);

		ListGridField extension = new ListGridField("extension", I18N.message("extension"), 80);
		extension.setCanEdit(false);
		extension.setValidators(validator);

		ListGridField name = new ListGridField("name", I18N.message("name"), 180);
		name.setCanEdit(false);
		name.setValidators(validator);

		ListGridField aliases = new ListGridField("aliases", I18N.message("aliases"));
		aliases.setCanEdit(true);
		aliases.setCanFilter(false);
		aliases.setCanSort(false);
		aliases.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return Util.strip(record.getAttributeAsString("aliases"));
			}
		});

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

		parsersList.addEditCompleteHandler(new EditCompleteHandler() {
			@Override
			public void onEditComplete(EditCompleteEvent event) {
				ListGridRecord record = parsersList.getRecord(event.getRowNum());

				SearchEngineService.Instance.get().setAliases(record.getAttributeAsString("extension"),
						(String) event.getNewValues().get("aliases"), new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								parsersList.invalidateCache();
								parsersList.getDataSource().invalidateCache();
								parsersList.redraw();
							}
						});
			}
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

		ListGridField enabled = new ListGridField("eenabled", " ", 24);
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
			langsList.addCellContextClickHandler(new CellContextClickHandler() {
				@Override
				public void onCellContextClick(CellContextClickEvent event) {
					showLanguagesMenu();
					event.cancel();
				}
			});

		return languagesTab;
	}

	private Tab fillFiltersTab() {
		ListGridField enabled = new ListGridField("eenabled", " ", 24);
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

		final ListGrid filtersGrid = new ListGrid() {
			@Override
			protected Canvas getExpansionComponent(final ListGridRecord record) {
				final String filter = record.getAttributeAsString("name");
				final ListGrid grid = this;

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
				ListGridField value = new ListGridField("value", I18N.message("value"));
				value.setWidth("*");
				value.setCanEdit(true);
				configsGrid.setFields(name, value);

				layout.addMember(configsGrid);

				HLayout hLayout = new HLayout(10);
				hLayout.setAlign(Alignment.CENTER);

				IButton saveButton = new IButton(I18N.message("save"));
				saveButton.setTop(250);
				saveButton.addClickHandler(new ClickHandler() {
					public void onClick(ClickEvent event) {
						final List<GUIParameter> params = new ArrayList<GUIParameter>();
						ListGridRecord[] records = configsGrid.getRecords();
						for (ListGridRecord rec : records) {
							params.add(new GUIParameter(rec.getAttributeAsString("name"), rec
									.getAttributeAsString("value")));
						}

						SearchEngineService.Instance.get().saveTokenFilterSettings(filter,
								params.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

									@Override
									public void onFailure(Throwable caught) {
										Log.serverError(caught);
									}

									@Override
									public void onSuccess(Void arg) {

									}
								});
					}
				});
				hLayout.addMember(saveButton);

				IButton closeButton = new IButton(I18N.message("close"));
				closeButton.addClickHandler(new ClickHandler() {
					public void onClick(ClickEvent event) {
						grid.collapseRecord(record);
					}
				});
				hLayout.addMember(closeButton);

				layout.addMember(hLayout);

				return layout;
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

		filtersGrid.addDropCompleteHandler(new DropCompleteHandler() {

			@Override
			public void onDropComplete(DropCompleteEvent event) {
				List<String> filters = new ArrayList<String>();
				ListGridRecord[] records = filtersGrid.getRecords();
				for (ListGridRecord record : records) {
					filters.add(record.getAttributeAsString("name"));
				}
				SearchEngineService.Instance.get().reorderTokenFilters(filters.toArray(new String[0]),
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {

							}
						});
			}
		});

		filtersGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showFilterMenu(filtersGrid);
				event.cancel();
			}
		});
		return filtersTab;
	}

	private void fillSearchEngineTab(GUISearchEngine searchEngine) {
		searchEngineTabPanel = new VLayout();
		searchEngineTabPanel.setWidth100();
		searchEngineTabPanel.setHeight100();

		DynamicForm searchEngineForm = new DynamicForm();
		searchEngineForm.setTitleOrientation(TitleOrientation.LEFT);
		searchEngineForm.setNumCols(2);
		searchEngineForm.setWrapItemTitles(false);
		searchEngineForm.setColWidths(1, "*");
		searchEngineForm.setValuesManager(vm);

		PickerIcon computeStat = new PickerIcon(new Picker("[SKIN]/picker_refresh.png"), new FormItemClickHandler() {
			public void onFormItemClick(final FormItemIconClickEvent event) {
				event.getItem().setValue(I18N.message("computing") + "...");
				SearchEngineService.Instance.get().countEntries(new AsyncCallback<Long>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Long count) {
						event.getItem().setValue(Util.formatLong(count));
					}
				});
			}
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
		StaticTextItem status = ItemFactory.newStaticTextItem(
				"status",
				"status",
				this.searchEngine.isLocked() ? ("<span style='color:red'>" + I18N.message("locked") + "</span>") : I18N
						.message("unlocked"));
		status.setRedrawOnChange(true);

		// Include Patters
		TextItem includePatters = ItemFactory.newTextItem("includePatters", "includepatters", null);
		includePatters.setValue(this.searchEngine.getIncludePatters());
		includePatters.setHint(I18N.message("separatedcomma"));
		includePatters.setHintStyle("hint");

		// Exclude Patters
		TextItem excludePatters = ItemFactory.newTextItem("excludePatters", "excludepatters", null);
		excludePatters.setValue(this.searchEngine.getExcludePatters());
		excludePatters.setHint(I18N.message("separatedcomma"));
		excludePatters.setHintStyle("hint");

		// The optional batch
		IntegerItem batch = ItemFactory.newIntegerItem("batch", "batch", this.searchEngine.getBatch());

		// The optional parse timeout
		IntegerItem timeout = ItemFactory.newIntegerItem("timeout", "parsingtimeout",
				this.searchEngine.getParsingTimeout());
		timeout.setHint(I18N.message("seconds"));

		// The optional max text that will be put in the index
		IntegerItem maxText = ItemFactory.newIntegerItem("maxtext", "maxtext", this.searchEngine.getMaxText());
		maxText.setHint(I18N.message("maxtextinindex"));

		// The optional max size elaborated when parsing text files
		IntegerItem maxTextFileSize = ItemFactory.newIntegerItem("maxtextfilesize", "maxtextfilesize",
				this.searchEngine.getMaxTextFileSize());
		maxTextFileSize.setHint(I18N.message("maxtextfilesizehint"));

		// Repository
		TextItem repository = ItemFactory.newTextItem("repository", "repository", null);
		repository.setValue(this.searchEngine.getDir());
		repository.setWidth(300);

		HLayout buttons = new HLayout();

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				@SuppressWarnings("unchecked")
				final Map<String, Object> values = vm.getValues();

				if (vm.validate()) {
					SearchIndexingPanel.this.searchEngine.setIncludePatters((String) values.get("includePatters"));
					SearchIndexingPanel.this.searchEngine.setExcludePatters((String) values.get("excludePatters"));
					SearchIndexingPanel.this.searchEngine.setDir((String) values.get("repository"));

					String btch = vm.getValueAsString("batch");
					if (btch == null || "".equals(btch.trim()))
						SearchIndexingPanel.this.searchEngine.setBatch(0);
					else
						SearchIndexingPanel.this.searchEngine.setBatch(new Integer(btch));

					String timeout = vm.getValueAsString("timeout");
					if (timeout == null || "".equals(timeout.trim()))
						SearchIndexingPanel.this.searchEngine.setParsingTimeout(0);
					else
						SearchIndexingPanel.this.searchEngine.setParsingTimeout(new Integer(timeout));

					String maxtext = vm.getValueAsString("maxtext");
					if (maxtext == null || "".equals(maxtext.trim()))
						SearchIndexingPanel.this.searchEngine.setMaxText(0);
					else
						SearchIndexingPanel.this.searchEngine.setMaxText(new Integer(maxtext));

					String maxTextFileSize = vm.getValueAsString("maxtextfilesize");
					if (maxtext == null || "".equals(maxtext.trim()))
						SearchIndexingPanel.this.searchEngine.setMaxTextFileSize(0);
					else
						SearchIndexingPanel.this.searchEngine.setMaxTextFileSize(new Integer(maxTextFileSize));

					SearchEngineService.Instance.get().save(SearchIndexingPanel.this.searchEngine,
							new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Log.info(I18N.message("settingssaved"), null);
									SearchEngineService.Instance.get().getInfo(new AsyncCallback<GUISearchEngine>() {

										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(GUISearchEngine searchEngine) {
											AdminScreen.get().setContent(new SearchIndexingPanel(searchEngine));
										}

									});
								}
							});
				}
			}
		});

		IButton unlock = new IButton();
		unlock.setTitle(I18N.message("unlock"));
		unlock.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				SearchEngineService.Instance.get().unlock(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						Log.info(I18N.message("indexunlocked"), null);
						SearchEngineService.Instance.get().getInfo(new AsyncCallback<GUISearchEngine>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUISearchEngine searchEngine) {
								AdminScreen.get().setContent(new SearchIndexingPanel(searchEngine));
							}

						});
					}
				});
			}
		});

		final IButton rescheduleAll = new IButton();
		rescheduleAll.setAutoFit(true);
		rescheduleAll.setTitle(I18N.message("rescheduleall"));
		rescheduleAll.addClickHandler(new ClickHandler() {

			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmreindex"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ContactingServer.get().show();
							rescheduleAll.setDisabled(true);
							SearchEngineService.Instance.get().rescheduleAll(false, new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									rescheduleAll.setDisabled(false);
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Log.info(I18N.message("docsreindex"), null);
									rescheduleAll.setDisabled(false);
									ContactingServer.get().hide();

									SearchEngineService.Instance.get().getInfo(new AsyncCallback<GUISearchEngine>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(GUISearchEngine searchEngine) {
											AdminScreen.get().setContent(new SearchIndexingPanel(searchEngine));
										}
									});
								}
							});
						}
					}
				});
			}
		});

		final IButton dropIndex = new IButton();
		dropIndex.setAutoFit(true);
		dropIndex.setTitle(I18N.message("dropindex"));
		dropIndex.addClickHandler(new ClickHandler() {

			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdropindex"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ContactingServer.get().show();
							rescheduleAll.setDisabled(true);
							SearchEngineService.Instance.get().rescheduleAll(true, new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									dropIndex.setDisabled(false);
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Log.info(I18N.message("docsreindex"), null);
									dropIndex.setDisabled(false);
									ContactingServer.get().hide();

									SearchEngineService.Instance.get().getInfo(new AsyncCallback<GUISearchEngine>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(GUISearchEngine searchEngine) {
											AdminScreen.get().setContent(new SearchIndexingPanel(searchEngine));
										}
									});
								}
							});
						}
					}
				});
			}
		});

		IButton check = new IButton();
		check.setTitle(I18N.message("check"));
		check.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				SearchEngineService.Instance.get().check(new AsyncCallback<String>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
						ContactingServer.get().hide();
					}

					@Override
					public void onSuccess(String ret) {
						ContactingServer.get().hide();
						SearchIndexCheckStatus sc = new SearchIndexCheckStatus(ret);
						sc.show();
					}
				});
			}
		});

		if (Session.get().isDefaultTenant()) {
			buttons.setMembers(save, unlock, rescheduleAll, dropIndex, check);
		} else {
			repository.setVisible(false);
			batch.setVisible(false);
			maxText.setVisible(false);
			buttons.setMembers(save, rescheduleAll);
		}

		searchEngineForm.setItems(entries, status, repository, includePatters, excludePatters, batch, timeout, maxText,
				maxTextFileSize);

		buttons.setMembersMargin(5);
		searchEngineTabPanel.setMembers(searchEngineForm, buttons);
		searchEngineTabPanel.setMembersMargin(15);
		searchEngineTabPanel.setMargin(5);

		body.setMembers(searchEngineTabPanel);
	}

	private void refreshIndexingQueue(Integer maxValue) {
		final IntegerItem max = ItemFactory.newValidateIntegerItem("max", "", maxValue, 1, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setWidth(60);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		display.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (max.validate())
					refreshIndexingQueue((Integer) max.getValue());
			}
		});

		// Prepare a panel containing a title and the documents number
		infoPanel = new InfoPanel("");

		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		ListGridField version = new ListGridField("version", I18N.message("version"), 55);
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(true);

		ListGridField lastModified = new ListGridField("lastModified", I18N.message("lastmodified"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);

		ListGridField publisher = new ListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);
		publisher.setCanFilter(true);

		ListGridField published = new ListGridField("published", I18N.message("publishedon"), 110);
		published.setAlign(Alignment.CENTER);
		published.setType(ListGridFieldType.DATE);
		published.setCellFormatter(new DateCellFormatter(false));
		published.setCanFilter(false);

		ListGridField creator = new ListGridField("creator", I18N.message("creator"), 90);
		creator.setAlign(Alignment.CENTER);
		creator.setCanFilter(true);

		ListGridField created = new ListGridField("created", I18N.message("createdon"), 110);
		created.setAlign(Alignment.CENTER);
		created.setType(ListGridFieldType.DATE);
		created.setCellFormatter(new DateCellFormatter(false));
		created.setCanFilter(false);

		ListGridField customId = new ListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setCanFilter(false);

		ListGridField locked = new ListGridField("locked", " ", 24);
		locked.setType(ListGridFieldType.IMAGE);
		locked.setCanSort(false);
		locked.setAlign(Alignment.CENTER);
		locked.setShowDefaultContextMenu(false);
		locked.setImageURLPrefix(Util.imagePrefix());
		locked.setImageURLSuffix(".png");
		locked.setCanFilter(false);

		ListGridField filename = new ListGridField("filename", I18N.message("filename"), 200);
		filename.setCanFilter(true);

		ListGridField lockUserId = new ListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);
		lockUserId.setCanFilter(false);

		docsList = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (record == null)
					return "";
				if (getFieldName(colNum).equals("filename")) {
					if ("stop".equals(record.getAttribute("immutable"))) {
						return "color: #888888; font-style: italic;";
					} else {
						return super.getCellCSSText(record, rowNum, colNum);
					}
				} else {
					return super.getCellCSSText(record, rowNum, colNum);
				}
			}
		};
		docsList.setEmptyMessage(I18N.message("notitemstoshow"));

		docsList.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showIndexQueueMenu();
				event.cancel();
			}
		});

		docsList.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showndocuments", Integer.toString(docsList.getTotalRows())));
			}
		});

		docsList.setShowRecordComponents(true);
		docsList.setShowRecordComponentsByCell(true);
		docsList.setCanFreezeFields(true);
		docsList.setAutoFetchData(true);
		docsList.setSelectionType(SelectionStyle.MULTIPLE);
		docsList.setShowFilterEditor(true);
		docsList.setFilterOnKeypress(true);
		dataSource = new DocumentsDS(null, null, maxValue, 1, 0, null);
		docsList.setDataSource(dataSource);
		docsList.setFields(locked, icon, filename, size, lastModified, version, publisher, published, creator, created,
				customId);

		if (indexingQueueTabPanel != null) {
			indexingQueueTabPanel.removeMembers(indexingQueueTabPanel.getMembers());
		} else {
			indexingQueueTabPanel = new VLayout();
			indexingQueueTabPanel.setWidth100();
			indexingQueueTabPanel.setHeight100();
		}

		indexingQueueTabPanel.setMembers(toolStrip, infoPanel, docsList);
	}

	private void showIndexQueueMenu() {
		final ListGridRecord[] selection = docsList.getSelectedRecords();

		Menu contextMenu = new Menu();
		MenuItem markUnindexable = new MenuItem();
		markUnindexable.setTitle(I18N.message("markunindexable"));
		markUnindexable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null)
					return;
				final long[] ids = new long[selection.length];
				for (int j = 0; j < selection.length; j++) {
					ids[j] = Long.parseLong(selection[j].getAttribute("id"));
				}

				DocumentService.Instance.get().markUnindexable(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : selection) {
							docsList.removeData(record);
						}
					}
				});
			}
		});

		contextMenu.setItems(markUnindexable);
		contextMenu.showContextMenu();
	}

	private void showLanguagesMenu() {
		final ListGridRecord record = langsList.getSelectedRecord();

		Menu contextMenu = new Menu();
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SearchEngineService.Instance.get().setLanguageStatus(record.getAttributeAsString("code"), true,
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								record.setAttribute("eenabled", "0");
								langsList.refreshRow(langsList.getRecordIndex(record));
								Log.info(I18N.message("settingsaffectnewsessions"), null);
							}
						});
			}
		});

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SearchEngineService.Instance.get().setLanguageStatus(record.getAttributeAsString("code"), false,
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								record.setAttribute("eenabled", "2");
								langsList.refreshRow(langsList.getRecordIndex(record));
								Log.info(I18N.message("settingsaffectnewsessions"), null);
							}
						});
			}
		});

		if ("0".equals(record.getAttributeAsString("eenabled")))
			contextMenu.setItems(disable);
		else
			contextMenu.setItems(enable);
		contextMenu.showContextMenu();
	}

	private void showFilterMenu(final ListGrid grid) {
		final ListGridRecord record = grid.getSelectedRecord();

		Menu contextMenu = new Menu();
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SearchEngineService.Instance.get().setTokenFilterStatus(record.getAttributeAsString("name"), true,
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								record.setAttribute("eenabled", "0");
								grid.refreshRow(grid.getRecordIndex(record));
							}
						});
			}
		});

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SearchEngineService.Instance.get().setTokenFilterStatus(record.getAttributeAsString("name"), false,
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								record.setAttribute("eenabled", "2");
								grid.refreshRow(grid.getRecordIndex(record));
							}
						});
			}
		});

		if ("0".equals(record.getAttributeAsString("eenabled")))
			contextMenu.setItems(disable);
		else
			contextMenu.setItems(enable);
		contextMenu.showContextMenu();
	}
}