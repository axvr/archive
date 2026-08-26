The navigation systems in Xamarin and Prism are primitive and missing lots of useful functionality.

Extension on Xamarin Shell or regular Xamarin (like Prism is).  .NET MAUI.

Router.MAUI

Statically typed/polymorphic navigation parameters.  (No dumb query parameters.)

Rejection of MVVM?  More Gait-like structure?  ***Router***

Better and more powerful API than all other alternatives.  A page route or file path is **not** a string.  It should be represented as a proper data structure.

```cs
await Navigate.Home()
              .Page<Switch>()
              .Page<MobileSwitch>()
              .Page<MobileHandsets>()
              .Go();
// await NavigationService.NavigateAsync("/NavigationPage/Home/MobileSwitch/MobileHandsets");

await Navigate.BackTo<Home>()
              .Page<Switch>()
              .Page<MobileSwitch>()
              .Page<MobileHandsets>()
              .Go();

// Can build partial navigations.
public static readonly IRoute HomeRoute = Router.BackTo<Home>(fallback: x => x.Reset().Page<Home>());
// `fallback` can specify fallback route if page is not in stack.
HomeRoute.Go();
// Router.Go(x => x.Route(HomeRoute));

// Partial route parameters.
public static IRoute Home(this IRoute route, string tab)
{
    var navParams = new NavParams { { "tab", tab } };
    return route.BackTo<Home>(navParams: navParams, fallback: x => x.Reset().Page<Home>(navParams));
}
public static IRoute CashbackDetails(this IRoute route, Guid sectorId, Guid offerId)
{
    var navParams = new NavParams { { "offerId", offerId } };
    return route.From<CashbackList>(sectorId)
                .Page<CashbackDetails>(navParams);
}
await Router.Go(x => x.Home(tab: "Earn")
                      .CashbackDetails(sectorId: _sectorId, offerId: offerId));

await Router.Go(x => x.CashbackDetails(sectorId, offerId));

await Router.Reset().Page<Home>().Go();

await Navigate(x => x.Home()
                     .Page<Switch>()
                     .Page<MobileSwitch>()
                     .Page<MobileHandsets>());

await Navigate.BackTo<Switch>().Go();
// Not possible in Prism.  By keeping track of the page stack,
// we can go back to the first instance of a specific page.

await Navigate.Back(new NavParams() { "Foo", "Bar" }).Route<SurveyList>().Go();
// Not possible in Prism.  We should be able to pass new navigation
// parameters to pages already in the navigation stack.  (We can
// somewhat do this already, using our custom Prism extension, but
// the API isn't as clean as in this example.)

// Back will drop the previous page from the stack.

Router.Register<..., ...>(...);

// Allow inspecting the current stack and replacing pages below.

// params: new NavParams(), mergeParams: true, fallback: null

// Navarams should be dictionary not a list of keyvalue pairs.

// Work perfectly with (and use?) LinqLisp.
// Everything is an extension method!  Very small lib, tiny core.

// Router.Current.Stack().Select(x => x.PageType.ToString());

public static IRoute From<TPage>(this IRoute route, NavParams navParams = null, bool mergeParams = true) where TPage : IPage
{
    return route
        .When(pred: x => x.PageType == TPage,
              then: x => x.Page<TPage>(navParams, mergeParams));
}

// Pass routes to pages as navigation parameters!  Tell the page how to get to the next page or what to do!
// E.g. drop a page from the stack if the user clicks this.

// Want a different navigation model, just write a few extension methods to implement it.

// No platform specific code required?  Abstraction on top of MAUI's default navigation.

// Faster than Prism?

// abstract BasePage : IRoutablePage

// CanNavigate alternative?  Null page, just execute code?

// Each page should know the previous stack and the route executed to get to current stack.

// INavigationAware?

// Navigation exceptions...  Know which page it failed on.

// Work with native Xamarin.Forms/MAUI MVVM system.

// Work on Xamarin.Forms?

// Event handlers for types of navigation events.
//   OnOpen
//   OnClose
//   OnRefresh
//   OnBack
// Run OnOpen on first page open even if navigating back.

// Prepass navigation parameters.
// e.g. when you see this page, pass it these parameters.
```
