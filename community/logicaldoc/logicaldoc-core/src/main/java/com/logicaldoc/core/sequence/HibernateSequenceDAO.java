package com.logicaldoc.core.sequence;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>SequenceDAO</code>. </p> Sequences are
 * implemented ad Generics whose type is 'sequence' and subtype is the sequence
 * name.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 4.0
 */
@SuppressWarnings("unchecked")
public class HibernateSequenceDAO extends HibernatePersistentObjectDAO<Sequence> implements SequenceDAO {

	private HibernateSequenceDAO() {
		super(Sequence.class);
		super.log = LoggerFactory.getLogger(HibernateSequenceDAO.class);
	}

	@Override
	public synchronized void reset(String sequence, long objectId, long tenantId, long value) {
		synchronized (SequenceDAO.class) {
			Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
			if (seq == null) {
				seq = new Sequence();
			}
			seq.setName(sequence);
			seq.setObjectId(objectId);
			seq.setTenantId(tenantId);
			seq.setLastReset(new Date());
			seq.setValue(value);
			store(seq);
		}
	}

	@Override
	public synchronized long next(String sequence, long objectId, long tenantId, long increment) {
		synchronized (SequenceDAO.class) {
			Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
			if (seq == null) {
				seq = new Sequence();
			}

			seq.setName(sequence);
			seq.setObjectId(objectId);
			seq.setTenantId(tenantId);
			seq.setValue(seq.getValue() + increment);
			store(seq);
			flush();
			return seq.getValue();
		}
	}

	@Override
	public synchronized long next(String sequence, long objectId, long tenantId) {
		return this.next(sequence, objectId, tenantId, 1L);
	}

	@Override
	public long getCurrentValue(String sequence, long objectId, long tenantId) {
		Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
		if (seq == null)
			return 0L;
		else
			return seq.getValue();
	}

	@Override
	public List<Sequence> findByName(String name, long tenantId) {
		String query = " _entity.tenantId=" + tenantId;
		query += " and _entity.name like '" + SqlUtil.doubleQuotes(name) + "%' ";
		return findByWhere(query, null, null);
	}

	@Override
	public Sequence findByAlternateKey(String name, long objectId, long tenantId) {
		try {
			Sequence sequence = null;

			String query = " _entity.tenantId = ?1 ";
			query += " and _entity.objectId = ?2 ";
			query += " and _entity.name = ?3 ";
			List<Sequence> sequences = new ArrayList<Sequence>();
			try {
				sequences = findByWhere(query, new Object[] { tenantId, objectId, name }, null, null);
			} catch (Throwable t) {
			}

			// It's incredible but the findByWhere sometimes doesn't find the
			// sequence so finding by the ID is safer
			if (sequences.isEmpty()) {
				query = "select ld_id from ld_sequence where ld_name='" + SqlUtil.doubleQuotes(name)
						+ "' and ld_objectid=" + objectId + " and ld_tenantid=" + tenantId;
				long sequenceId = queryForLong(query);
				if (sequenceId != 0L)
					sequence = findById(sequenceId);
			} else {
				sequence = sequences.get(0);
			}

			if (sequence == null)
				log.debug("Unable to find sequence " + name + "," + objectId + "," + tenantId);
			else
				refresh(sequence);

			return sequence;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}

	@Override
	public void delete(String name, long objectId, long tenantId) {
		Sequence seq = findByAlternateKey(name, objectId, tenantId);
		if (seq != null)
			delete(seq.getId());
	}

	@Override
	public boolean delete(long id, int code) {
		Sequence seq = findById(id);
		if (seq != null) {
			seq.setName(seq.getId() + "." + seq.getName());
			seq.setDeleted(code);
			store(seq);
		}
		return true;
	}
}