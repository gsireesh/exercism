class Matrix:

	def __init__(self, pMatString):
		self.rows = self.rowify(pMatString)
		#self.columns = self.columnify(pMatString)

	def rowify(self, pMatString):
		#getting the correct structure
		temp = [i.split(' ') for i in pMatString.split('\n')]
		temp2 = []
		#converting to right data type
		for row in temp:
			temp2.append(list(map(int, row)))
		return temp2

	def columnify(self, pMatString):
		
