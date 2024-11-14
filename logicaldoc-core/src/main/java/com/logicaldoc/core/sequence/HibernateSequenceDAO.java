package com.logicaldoc.core.sequence;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernatePersistentObjectDAO;

/**
 * Hibernate implementation of <code>SequenceDAO</code>. <br>
 * Sequences are implemented ad Generics whose type is 'sequence' and subtype is
 * the sequence name.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateSequenceDAO extends HibernatePersistentObjectDAO<Sequence> implements SequenceDAO {

	private static final String TENANTID = "tenantId";
	private static final String AND = " and ";

	private HibernateSequenceDAO() {
		super(Sequence.class);
		super.log = LoggerFactory.getLogger(HibernateSequenceDAO.class);
	}

	@Override
	public synchronized void reset(String sequence, long objectId, long tenantId, long value) {
		synchronized (SequenceDAO.class) {
			Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
			if (seq == null)
				seq = new Sequence();
			seq.setName(sequence);
			seq.setObjectId(objectId);
			seq.setTenantId(tenantId);
			seq.setLastReset(new Date());
			seq.setValue(value);

			try {
				store(seq);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
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
			try {
				store(seq);
				flush();
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
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
		String query = " " + ENTITY + ".tenantId = :tenantId " + AND + ENTITY + ".name like :name ";

		try {
			return findByWhere(query, Map.of(TENANTID, tenantId, "name", name + "%"), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public Sequence findByAlternateKey(String name, long objectId, long tenantId) {
		try {
			List<Sequence> sequences = findSequences(name, objectId, tenantId);

			Sequence sequence = null;

			// It's incredible but the findByWhere sometimes doesn't find the
			// sequence so finding by the ID is safer
			if (sequences.isEmpty()) {
				sequence = findSequence(name, objectId, tenantId, sequence);
			} else {
				sequence = sequences.get(0);
			}

			if (sequence == null)
				log.debug("Unable to find sequence {},{},{}", name, objectId, tenantId);
			else
				refresh(sequence);

			return sequence;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}

	private Sequence findSequence(String sequenceName, long objectId, long tenantId, Sequence sequence) {
		String query = "select ld_id from ld_sequence where ld_name = :name and ld_objectid = :objectId and ld_tenantid = :tenantId";
		try {
			long sequenceId = queryForLong(query,
					Map.of(TENANTID, tenantId, "objectId", objectId, "name", sequenceName));
			if (sequenceId != 0L)
				sequence = findById(sequenceId);
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}
		return sequence;
	}

	private List<Sequence> findSequences(String sequenceName, long objectId, long tenantId) {
		List<Sequence> sequences = new ArrayList<>();
		try {
			String query = " " + ENTITY + ".tenantId = :tenantId ";
			query += AND + ENTITY + ".objectId = :objectId ";
			query += AND + ENTITY + ".name = :name ";

			sequences = findByWhere(query, Map.of(TENANTID, tenantId, "objectId", objectId, "name", sequenceName),
					null, null);
		} catch (Exception t) {
			// Nothing to do
		}
		return sequences;
	}

	@Override
	public void delete(String name, long objectId, long tenantId) throws PersistenceException {
		Sequence seq = findByAlternateKey(name, objectId, tenantId);
		if (seq != null)
			delete(seq.getId());
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		Sequence seq = findById(id);
		if (seq != null) {
			seq.setName(seq.getId() + "." + seq.getName());
			seq.setDeleted(code);
			store(seq);
		}
	}
}