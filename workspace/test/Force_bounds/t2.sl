// Estruturas aninhadas

struct Word {
    numChars: int;
    content: string;
}

struct Paragraf {
    content: Word[];
    font: string;
}

struct Page {
    content: Paragraf[];
    typePaper: string;
}

struct Book {
    Title: string;
    CoverMaterial: string;
    pages: Page[];
}

struct Shelve {
    Id: int;
    books: Book[];
}

struct Section {
    numShelves: int;
    genre: string;
    Shelves: Shelve[];
}

struct BookStore {
    address: string;
    Sections: Section[];
}

struct Franchise {
    name: string;
    profit: int;
    BookStores: BookStore[];

}

func main(): int{

    let Sample_contents: Word[4];
    Sample_contents[0] = {5, "Magic"};
    Sample_contents[1] = {2, "is"};
    Sample_contents[2] = {3, "not"};
    Sample_contents[3] = {4, "real"};

    let Sample_paragfs: Paragraf[1];
    Sample_paragfs[0] = {Sample_contents, "Arial 12"};

    let Sample_pages: Page[1];
    Sample_pages[0] = {Sample_paragfs, "Yellow paper"};

    let Sample_book: Book[1];
    Sample_book[0] = Book{"ComplacencyOfTheLearned", "Leather", Sample_pages};

    let BookStoreShelves1: Shelve[1];
    BookStoreShelves1[0] = Shelve{1, Sample_book};

    let BookStoreShelves2: Shelve[1];
    BookStoreShelves2[0] = Shelve{2, Sample_book};

    let BookStoreSections: Section[2];
    BookStoreSections[0] = Section{2, "Romance", BookStoreShelves};
    BookStoreSections[1] = Section{2, "Matematica", BookStoreShelves};

    let BookStoreLispector: BookStore[2];
    BookStoreLispector[0] = BookStore{"Ouro Preto", BookStoreSections};
    BookStoreLispector[1] = BookStore{"Ouro Preto", BookStoreSections};

    let Lispector: Franchise{"Lispector", 10000, BookStoreLispector};
    printf(Lispector.BookStores[0].Sections[0].Shelves[0].books[0].pages[0].content[0].content[0].content[0]);

    return 0;
}