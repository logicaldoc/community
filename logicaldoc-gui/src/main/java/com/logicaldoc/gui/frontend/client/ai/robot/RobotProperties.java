package com.logicaldoc.gui.frontend.client.ai.robot;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.model.ModelsDS;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Shows robot's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotProperties extends RobotDetailsTab {

	private static final String CLASSIFIER = "classifier";

	private static final String LABEL = "label";

	private static final String CATEGORY = "category";

	private static final String ANSWER = "answer";

	private static final String ID = "id";

	private static final String NAME = "name";

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	private ListGrid answers;

	private ListGridRecord rollOverRecord;

	private HLayout rollOverCanvas;

	public RobotProperties(GUIRobot robot, final ChangedHandler changedHandler) {
		super(robot, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(container);

		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(4);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem name = ItemFactory.newSimpleTextItem(NAME, robot.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);

		TextItem label = ItemFactory.newTextItem(LABEL, robot.getLabel());
		label.addChangedHandler(changedHandler);

		SelectItem classifier = new SelectItem(CLASSIFIER, I18N.message(CLASSIFIER));
		classifier.setDisplayField(LABEL);
		classifier.setValueField("id");
		classifier.setWidth(150);
		classifier.setMultiple(false);
		classifier.setWrapTitle(false);
		classifier.setRequired(true);
		classifier.setStartRow(true);
		classifier.setMultipleAppearance(MultipleAppearance.PICKLIST);
		classifier.setOptionDataSource(new ModelsDS(CLASSIFIER));
		if (robot.getClassifierId() != 0L)
			classifier.setValue(robot.getClassifierId());
		classifier.addChangedHandler(changedHandler);

		SelectItem tokensDetector = new SelectItem("tokensDetector", I18N.message("tokensdetector"));
		tokensDetector.setDisplayField(LABEL);
		tokensDetector.setValueField("id");
		tokensDetector.setWidth(150);
		tokensDetector.setMultiple(false);
		tokensDetector.setWrapTitle(false);
		tokensDetector.setRequired(true);
		tokensDetector.setMultipleAppearance(MultipleAppearance.PICKLIST);
		tokensDetector.setOptionDataSource(new ModelsDS("tokens"));
		if (robot.getTokensDetectorId() != 0L)
			tokensDetector.setValue(robot.getTokensDetectorId());
		tokensDetector.addChangedHandler(changedHandler);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", robot.getDescription());
		description.addChangedHandler(changedHandler);
		description.setColSpan(4);
		description.setWidth("*");

		StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(robot.getId()));
		id.setVisible(robot.getId() != 0L);

		form.setItems(id, name, label, classifier, tokensDetector, description);

		container.setMembersMargin(3);
		container.addMember(form);
		prepareAnswers();
		prepareAvatar();
	}

	boolean validate() {
		if (form.validate()) {
			robot.setName(form.getValueAsString(NAME));
			robot.setLabel(form.getValueAsString(LABEL));
			robot.setDescription(form.getValueAsString("description"));
			robot.setClassifierId(Long.parseLong(form.getValueAsString(CLASSIFIER)));
			robot.setTokensDetectorId(Long.parseLong(form.getValueAsString("tokensDetector")));

			com.smartgwt.client.data.Record[] answerRecords = answers.getRecordList().toArray();
			robot.getAnswers().clear();
			for (com.smartgwt.client.data.Record answerRecord : answerRecords)
				robot.getAnswers()
						.add(new GUIValue(answerRecord.getAttribute(CATEGORY), answerRecord.getAttribute(ANSWER)));
		}
		return !form.hasErrors();
	}

	private void prepareAnswers() {
		answers = new ListGrid() {
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
					editImg.setSrc("[SKIN]/icons/pencil.png");
					editImg.setPrompt(I18N.message("edit"));
					editImg.setHeight(16);
					editImg.setWidth(16);
					editImg.addClickHandler(event -> onEdit());

					rollOverCanvas.addMember(editImg);
				}
				return rollOverCanvas;

			}
		};
		answers.setEmptyMessage(I18N.message("notitemstoshow"));
		answers.setWidth100();
		answers.setHeight100();
		answers.setEmptyMessage(I18N.message("norecords"));
		answers.setCanSort(false);
		answers.setCanFreezeFields(false);
		answers.setCanGroupBy(false);
		answers.setLeaveScrollbarGap(false);
		answers.setShowHeader(true);
		answers.setSelectionType(SelectionStyle.MULTIPLE);
		answers.setCanEdit(false);
		answers.setShowRowNumbers(false);
		answers.setCanReorderRecords(true);
		answers.setAutoFetchData(true);
		answers.setShowRecordComponents(true);
		answers.setShowRecordComponentsByCell(true);
		answers.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		answers.addDoubleClickHandler(click -> onEdit());

		ListGridField name = new ListGridField(CATEGORY, I18N.message(CATEGORY));
		name.setCanEdit(true);
		name.setCanSort(true);
		name.setAutoFitWidth(true);
		name.setMinWidth(150);
		name.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		ListGridField answer = new ListGridField(ANSWER, I18N.message(ANSWER));
		answer.setCanEdit(true);
		answer.setCanSort(false);
		answer.setWidth("*");

		answers.setFields(name, answer);

		// Initialize the answers grid
		for (GUIValue val : robot.getAnswers()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(CATEGORY, val.getCode());
			rec.setAttribute(ANSWER, val.getValue());
			answers.addData(rec);
		}

		SectionStack answersStack = new SectionStack();
		answersStack.setHeight100();

		IButton addLayer = new IButton(I18N.message("addanswer"));
		addLayer.addClickHandler(click -> {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(CATEGORY, CATEGORY);
			rec.setAttribute(ANSWER, "");
			answers.addData(rec);
			changedHandler.onChanged(null);
		});

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("answers") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);
		section.setItems(answers, addLayer);

		answersStack.setSections(section);
		answersStack.draw();

		container.addMember(answersStack);
	}

	private void onEdit() {
		new AnswerEditor(answers, rollOverRecord, changedHandler).show();
	}

	private void prepareAvatar() {
		if (robot.getId() != 0L) {
			RobotAvatar avatar = new RobotAvatar(robot.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(String avatar) {
					robot.setAvatar(avatar);
				}
			});

			SectionStackSection section = new SectionStackSection("<b>" + I18N.message("avatar") + "</b>");
			section.setCanCollapse(false);
			section.setExpanded(true);
			section.setItems(avatar);

			SectionStack avatarStack = new SectionStack();
			avatarStack.setWidth(Session.get().getConfigAsInt("gui.avatar.size") + 5);
			avatarStack.setSections(section);
			avatarStack.draw();
			container.addMember(avatarStack);
		}
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(click -> {
			answers.removeSelectedData();
			changedHandler.onChanged(null);
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
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