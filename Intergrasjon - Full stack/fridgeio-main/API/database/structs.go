package database

type UserData struct {
	UserD       User                 `json:"user"`
	Products    []Product            `json:"products"`
	UserLists   []UsersLists         `json:"userlists"`
	Recipes     []Recipe             `json:"recipes"`
	RecipeInfos []RecipeWithProducts `json:"recipeinfos"`
}

type User struct {
	Username string `json:"username"`
	Email    string `json:"email"`
	Password string `json:"password"`
	Token    string `json:"sessiontoken"`
}

type Product struct {
	Id          int    `json:"id"`
	Name        string `json:"productname"`
	BaseMeasure string `json:"measure"`
	TypeFood    string `json:"type"`
}

type UsersLists struct {
	UserId         string
	ProductId      int     `json:"productid"`
	ExpirationDate string  `json:"expiration"`
	Amount         float64 `json:"amount"`
	ImageId        int     `json:"imageid"`
	Favorite       bool    `json:"favorite"`
}

type UsersRecipe struct {
	UserId   int `json:"userid"`
	RecipeId int `json:"recipeid"`
}
type RecipeProduct struct {
	RecipeId int     `json:"recipeid"`
	Id       int     `json:"id"`
	Amount   float64 `json:"amount"`
}
type Recipe struct {
	Id          int    `json:"recipeid"`
	Name        string `json:"recipename"`
	Description string `json:"recipedescription"`
	Portion     int    `json:"portion"`
}

type UserAddItem struct {
	TUser    User       `json:"user"`
	TProduct Product    `json:"product"`
	TUList   UsersLists `json:"userlist"`
}
type RecipeWithProducts struct {
	Recipe
	Products []Product
	Amounts  []float64
}

type UserAddRecipe struct {
	TUser             User      `json:"user"`
	TRecipe           Recipe    `json:"recipe"`
	TRequiredProducts []int     `json:"trequiredproducts"`
	TAmount           []float64 `json:"tamount"`
}

type UserDeleteRecipe struct {
	TUser   User   `json:"user"`
	TRecipe Recipe `json:"recipe"`
}
