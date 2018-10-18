      db.run(peopleTable.map(_.fName).groupBy())
